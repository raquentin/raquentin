use dialoguer::{theme::ColorfulTheme, MultiSelect, Input};
use std::fs::{self};
use std::path::PathBuf;
use tempfile::tempdir;

mod utils;

fn main() {
    println!("entitled-exe v0.3.1");
    
    let github_repo: String = Input::with_theme(&ColorfulTheme::default())
        .with_prompt("Enter the GitHub repository (<user/repo>)")
        .interact_text()
        .unwrap();

    let github_url = format!("https://github.com/{}", github_repo);

    let repo_parts: Vec<&str> = github_repo.split('/').collect();
    let repo_name = repo_parts.last().unwrap_or(&"default_repo_name");
    let repo_name = repo_name.replace("/", "-");
    
    let sleep_duration: u64 = Input::with_theme(&ColorfulTheme::default())
        .with_prompt("Enter the sleep duration in milliseconds (ms)")
        .default(0u64)
        .interact_text()
        .unwrap();

    let version: String = Input::with_theme(&ColorfulTheme::default())
        .with_prompt("Enter the semantic version")
        .default("1.0.0".to_string())
        .interact_text()
        .unwrap();
    
    let output_directory: PathBuf = Input::with_theme(&ColorfulTheme::default())
        .with_prompt("Enter the output directory")
        .default("builds".to_string())
        .interact_text()
        .map(PathBuf::from)
        .unwrap();
    
    let available_targets = vec![
        "x86_64-pc-windows-gnu",
        "x86_64-unknown-linux-gnu",
    ];

    let selections = MultiSelect::with_theme(&ColorfulTheme::default())
        .with_prompt("Select targets to compile for")
        .items(&available_targets)
        .interact()
        .unwrap();

    if selections.is_empty() {
        println!("No targets selected, exiting.");
        return;
    }

    fs::create_dir_all(&output_directory).expect("Failed to create output directory");

    for &selection in &selections {
        let target = available_targets[selection];

        if !utils::is_target_installed(target) {
            println!("The target {} is not installed. Please install it using `rustup target add {}`.", target, target);
            return;
        }

        let dir = tempdir().unwrap();
        let project_path = dir.path();

        utils::init_cargo_project(project_path, &repo_name);
        utils::write_main_rs(project_path, &github_url, sleep_duration);
        utils::add_dependencies_to_cargo_toml(project_path);

        println!("Compiling for: {}", target);
        if utils::compile_project(project_path, target) {
            println!("Successfully compiled for target: {}", target);

            match utils::package_executable(project_path, &repo_name, target, &version, &output_directory) {
                Ok(_) => println!("Compiled and zipped for target: {}", target),
                Err(e) => eprintln!("{}", e),
            }
        } else {
            eprintln!("Compilation failed for target: {}", target);
        }
    }
}
