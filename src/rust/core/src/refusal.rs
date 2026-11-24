//! R wording for the steps of a pharos [`Refusal`].
//!
//! pharos names the ways forward as data; each front end words them. These
//! are the R calls, with the project chosen through `options(hyperion.config_dir)`.

use nonmem::scm::shown;
use nonmem::{Refusal, Step};

fn step_text(step: &Step) -> String {
    match step {
        Step::Overwrite => "call again with `overwrite = TRUE` to replace it".to_string(),
        Step::GiveBasePath => {
            "call again with `base_path` set to the project root the runs were recorded under"
                .to_string()
        }
        Step::UseProject { root } => format!(
            "start R in that project, or set `options(hyperion.config_dir = \"{}\")`",
            root.display()
        ),
        Step::FollowScm { config, .. } => {
            format!("follow it with `get_scm_status(\"{}\")`", shown(config))
        }
        Step::StopScm { config, .. } => {
            format!("stop it first with `stop_scm(\"{}\")`", shown(config))
        }
    }
}

/// `text`, then on a line of their own the steps of a refusal in `e`'s
/// chain, worded as R calls and joined as alternatives. A refusal with no
/// steps, or any other error, adds nothing.
pub fn with_r_steps(text: String, e: &anyhow::Error) -> String {
    match e.downcast_ref::<Refusal>().filter(|r| !r.steps.is_empty()) {
        Some(refusal) => {
            let steps: Vec<String> = refusal.steps.iter().map(step_text).collect();
            format!("{text}\n{}", steps.join(", or "))
        }
        None => text,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use anyhow::Context;
    use std::path::PathBuf;

    fn refusal(steps: Vec<Step>) -> anyhow::Error {
        anyhow::Error::new(Refusal {
            problem: "the problem".to_string(),
            steps,
        })
    }

    #[test]
    fn words_each_step_as_an_r_call() {
        let e = refusal(vec![
            Step::Overwrite,
            Step::GiveBasePath,
            Step::UseProject {
                root: PathBuf::from("/proj"),
            },
        ]);
        assert_eq!(
            with_r_steps("text".to_string(), &e),
            "text\ncall again with `overwrite = TRUE` to replace it, or call again with `base_path` set to the project root the runs were recorded under, or start R in that project, or set `options(hyperion.config_dir = \"/proj\")`"
        );
    }

    #[test]
    fn names_the_config_for_scm_steps() {
        let config = PathBuf::from("/no/such/project/scm.toml");
        let e = refusal(vec![
            Step::FollowScm {
                out_dir: PathBuf::from("/no/such/project/scm"),
                config: config.clone(),
            },
            Step::StopScm {
                out_dir: PathBuf::from("/no/such/project/scm"),
                config,
            },
        ]);
        let shown = shown(&PathBuf::from("/no/such/project/scm.toml"));
        assert_eq!(
            with_r_steps("text".to_string(), &e),
            format!(
                "text\nfollow it with `get_scm_status(\"{shown}\")`, or stop it first with `stop_scm(\"{shown}\")`"
            )
        );
    }

    #[test]
    fn finds_a_refusal_under_context() {
        let e = Err::<(), _>(refusal(vec![Step::Overwrite]))
            .context("outer")
            .unwrap_err();
        assert_eq!(
            with_r_steps("text".to_string(), &e),
            "text\ncall again with `overwrite = TRUE` to replace it"
        );
    }

    #[test]
    fn puts_the_steps_under_text_over_several_lines() {
        let e = refusal(vec![Step::Overwrite]);
        assert_eq!(
            with_r_steps("line one\nline two".to_string(), &e),
            "line one\nline two\ncall again with `overwrite = TRUE` to replace it"
        );
    }

    #[test]
    fn leaves_other_errors_and_stepless_refusals_alone() {
        let plain = anyhow::anyhow!("plain");
        assert_eq!(with_r_steps("text".to_string(), &plain), "text");
        let stepless = refusal(vec![]);
        assert_eq!(with_r_steps("text".to_string(), &stepless), "text");
    }
}
