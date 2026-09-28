use std::fmt;
use std::path::Path;

use extendr_api::Result;
use extendr_api::prelude::*;
use fs_err as fs;

use hyperion_core::OptionExt;

use crate::utils::{path_from_robj, resolve_model_run};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RunStatus {
    Run,
    Running,
    NotRun,
}

impl fmt::Display for RunStatus {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let value = match self {
            RunStatus::Run => "run",
            RunStatus::Running => "running",
            RunStatus::NotRun => "not_run",
        };
        f.write_str(value)
    }
}

/// Determine the run status from on-disk outputs.
///
/// `Run` requires the `.lst` to contain NONMEM's "Stop Time:" marker, which is
/// written at run termination regardless of whether estimation or covariance
/// succeeded. `Running` means the `.lst` exists but the marker is absent.
/// `NotRun` means neither the run directory nor the `.lst` exists.
pub fn determine_run_status(run_dir: &Path, stem: &str) -> Result<RunStatus> {
    if !run_dir.exists() {
        return Ok(RunStatus::NotRun);
    }
    let lst_path = run_dir.join(format!("{stem}.lst"));
    if !lst_path.exists() {
        return Ok(RunStatus::NotRun);
    }
    if lst_indicates_completion(&lst_path) {
        return Ok(RunStatus::Run);
    }
    Ok(RunStatus::Running)
}

fn lst_indicates_completion(lst_path: &Path) -> bool {
    let Ok(content) = fs::read_to_string(lst_path) else {
        return false;
    };
    content
        .lines()
        .rev()
        .any(|line| line.trim_start().starts_with("Stop Time:"))
}

/// Determine run status for a model path, run directory, or model object.
///
/// @param input A hyperion_nonmem_model object, run directory, or model path.
/// @return "run", "running", or "not_run"
///
/// Accepts .mod/.ctl/.lst paths, run directories, or a hyperion_nonmem_model object.
#[extendr]
pub fn get_run_status(input: Robj) -> Result<Robj> {
    let path = path_from_robj(&input, false)?;
    Ok(determine_model_run_status(&path)?.to_string().into_robj())
}

/// Shared by read_model() and status refreshes so both use the same run layout.
pub fn determine_model_run_status(path: &Path) -> Result<RunStatus> {
    // A standalone listing can report status without a source control stream.
    if path.extension().and_then(|ext| ext.to_str()) == Some("lst") {
        let stem = path
            .file_stem()
            .ok_or_extendr_err("Could not determine model file stem")?;
        let parent = path
            .parent()
            .ok_or_extendr_err("Could not determine model file parent directory")?;
        return determine_run_status(parent, &stem.to_string_lossy());
    }
    let (layout, run_dir) = resolve_model_run(path)?;
    determine_run_status(&run_dir, layout.stem())
}

extendr_module! {
   mod run_status;

    fn get_run_status;
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use std::path::PathBuf;
    use tempfile::TempDir;

    fn test_data_dir() -> PathBuf {
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("test_data")
    }

    #[test]
    fn test_determine_run_status_run() {
        let run_dir = test_data_dir().join("run001");
        let status = determine_run_status(&run_dir, "run001").unwrap();
        assert_eq!(status, RunStatus::Run);
    }

    #[test]
    fn test_determine_run_status_running() {
        let run_dir = test_data_dir().join("run001-running");
        let status = determine_run_status(&run_dir, "run001").unwrap();
        assert_eq!(status, RunStatus::Running);
    }

    #[test]
    fn test_determine_run_status_not_run() {
        // run_dir doesn't exist
        let temp_dir = TempDir::new().unwrap();
        let run_dir = temp_dir.path().join("run001");
        let status = determine_run_status(&run_dir, "run001").unwrap();
        assert_eq!(status, RunStatus::NotRun);
    }

    #[test]
    fn test_determine_run_status_running_early() {
        // .lst exists without "Stop Time:" => still running
        let temp_dir = TempDir::new().unwrap();
        let run_dir = temp_dir.path().join("run001");
        fs::create_dir(&run_dir).unwrap();
        fs::write(run_dir.join("run001.lst"), "partial lst content\n").unwrap();
        let status = determine_run_status(&run_dir, "run001").unwrap();
        assert_eq!(status, RunStatus::Running);
    }

    #[test]
    fn test_determine_run_status_not_run_empty_run_dir() {
        // run_dir exists but no .lst inside
        let temp_dir = TempDir::new().unwrap();
        let run_dir = temp_dir.path().join("run001");
        fs::create_dir(&run_dir).unwrap();
        let status = determine_run_status(&run_dir, "run001").unwrap();
        assert_eq!(status, RunStatus::NotRun);
    }
}
