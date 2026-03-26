use extendr_api::prelude::*;

pub mod nmmodel;
pub mod utils;

// Generate extendr module for R integration
extendr_module! {
    mod hyperion_nmparser;
    use nmmodel;
}
