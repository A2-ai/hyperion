use extendr_api::Result;
use extendr_api::prelude::*;

use fs_err as fs;
use nonmem::MigrationReport;
use std::path::Path;

use hyperion_core::{OptionExt, ResultExt, extendr_err, find_config_dir};

/// Migrate pharos run start files to project-relative model paths
///
/// Rewrites every `pharos_start.json` under the project root, replacing the
/// absolute `model_canonical_path` written by older pharos versions with a
/// `model_path` relative to the project root. Files already in the current
/// format are left untouched. `.git` and `rv` directories are skipped.
///
/// @param base_path Project root the runs were originally recorded under.
/// Only needed when the recorded absolute paths fall outside the current
/// project root, for example runs executed from another user's home
/// directory.
///
/// @return nothing
/// @export
///
/// @examples \dontrun{
/// migrate_run_start_files()
/// migrate_run_start_files(base_path = "/data/user-homes/analyst1/project-root")
/// }
#[extendr]
pub fn migrate_run_start_files(#[extendr(default = "NULL")] base_path: Option<&str>) -> Result<()> {
    let project_root = find_config_dir()?
        .ok_or_extendr_err("No pharos.toml found; migration requires a pharos project root")?;
    // Canonicalize to match the absolute paths recorded in the start files.
    let project_root = fs::canonicalize(&project_root)
        .map_to_extendr_err("Failed to canonicalize project root")?;

    // base_path is left as given; it may name a root that does not exist here.
    let report = MigrationReport::migrate_run_start_files(&project_root, base_path.map(Path::new))
        .map_to_extendr_err("Failed to migrate run start files")?;

    rprintln!(
        "{} file(s) migrated, {} already migrated",
        report.migrated,
        report.skipped
    );

    if !report.failed.is_empty() {
        let details = report
            .failed
            .iter()
            .map(|(path, reason)| format!(" - {}: {reason}", path.display()))
            .collect::<Vec<_>>()
            .join("\n");
        return Err(extendr_err!(
            "Failed to migrate {} file(s):\n{details}",
            report.failed.len()
        ));
    }

    Ok(())
}

extendr_module! {
    mod migrate;
    fn migrate_run_start_files;
}
