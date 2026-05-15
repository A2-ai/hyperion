use extendr_api::Result;
use extendr_api::prelude::*;
use std::path::PathBuf;

//pharos config crate
use config::{CONFIG_FILENAME, find_config_dir as pharos_find_config_dir};

// Trait extensions for mapping error to extendr_api::Error::Other
// with custom message preceding the error message.
pub trait ResultExt<T> {
    fn map_to_extendr_err(self, message: impl Into<String>) -> Result<T>;
}

impl<T, E: std::fmt::Debug> ResultExt<T> for std::result::Result<T, E> {
    fn map_to_extendr_err(self, message: impl Into<String>) -> Result<T> {
        self.map_err(|x| extendr_api::Error::Other(format!("{}: {x:?}", message.into())))
    }
}

pub trait OptionExt<T> {
    fn ok_or_extendr_err(self, message: impl Into<String>) -> Result<T>;
}

impl<T> OptionExt<T> for Option<T> {
    fn ok_or_extendr_err(self, message: impl Into<String>) -> Result<T> {
        self.ok_or_else(|| Error::Other(message.into()))
    }
}

#[macro_export]
macro_rules! extendr_err {
    ($($arg:tt)*) => {
        Error::Other(format!($($arg)*))
    };
}

/// Name of the R option that overrides pharos's CWD-walk for the project
/// root. When set to a non-empty string, `find_config_dir` returns that
/// path verbatim — `pharos.toml` is not required to live there. Useful
/// for tests and scripts that don't want hyperion to depend on CWD.
pub const CONFIG_DIR_OPTION: &str = "hyperion.config_dir";

pub fn find_config_dir() -> Result<Option<PathBuf>> {
    if let Some(path) = config_dir_from_option() {
        return Ok(Some(path));
    }
    pharos_find_config_dir().map_to_extendr_err("Failed to find config dir")
}

/// Read the `hyperion.config_dir` R option. Returns `None` when the
/// option is unset, NULL, or an empty string.
fn config_dir_from_option() -> Option<PathBuf> {
    let opt = call!("getOption", CONFIG_DIR_OPTION).ok()?;
    let s = opt.as_str().filter(|s| !s.is_empty())?;
    Some(PathBuf::from(s))
}

#[extendr]
pub fn silence_panic_output() {
    std::panic::set_hook(Box::new(|_| {}));
}

#[extendr]
pub fn find_pharos_config_file() -> Result<Robj> {
    let config_dir = find_config_dir()?;

    match config_dir {
        Some(d) => Ok(d.join(CONFIG_FILENAME).to_string_lossy().into_robj()),
        None => Ok(
            "No pharos.toml config file found. Please call hyperion::init() to create one"
                .into_robj(),
        ),
    }
}

extendr_module! {
    mod hyperion_core;

    fn silence_panic_output;
    fn find_pharos_config_file;
}
