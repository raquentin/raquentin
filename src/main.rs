use dialoguer::{theme::ColorfulTheme, MultiSelect, Input};
use std::fs::{File, self};
use std::path::{PathBuf, Path};
use std::io::Write;
use std::process::Command;
use tempfile::tempdir;
use zip::write::{FileOptions, ZipWriter};

fn main() {
    let github_repo: String = Input::with_theme(&ColorfulTheme::default())
        .with_prompt("Enter the GitHub repository (<user/repo>)")
        .interact_text()
        .unwrap();

    let github_url = format!("https://github.com/{}", github_repo);

    let repo_parts: Vec<&str> = github_repo.split('/').collect();
    let repo_name = repo_parts.last().unwrap_or(&"default_repo_name"); // Fallback in case of unexpected format
    let repo_name = repo_name.replace("/", "-"); // Sanitize, just in case
    
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

    let output_directory = PathBuf::from("builds");
    fs::create_dir_all(&output_directory).expect("Failed to create output directory");

    for &selection in &selections {
        let target = available_targets[selection];

        if !is_target_installed(target) {
            println!("The target {} is not installed. Please install it using `rustup target add {}`.", target, target);
            return;
        }
        
        // Create a temporary directory for the cargo project
        let dir = tempdir().unwrap();
        let project_path = dir.path();

        // Initialize a new Cargo project
        Command::new("cargo")
            .arg("init")
            .arg("--name")
            .arg("github_opener")
            .arg(project_path.to_str().unwrap())
            .status()
            .expect("Failed to create temporary Cargo project");

        // Create the Rust source file
        let source_code = format!(
            r#"use webbrowser;
            fn main() {{
                webbrowser::open("{}").unwrap();
            }}"#,
            github_url
        );

        let src_path = project_path.join("src");
        let main_rs = src_path.join("main.rs");
        fs::write(main_rs, source_code).expect("Failed to write main.rs");

        // Add dependencies to Cargo.toml
        let cargo_toml_path = project_path.join("Cargo.toml");
        let mut cargo_toml = fs::OpenOptions::new()
            .append(true)
            .open(cargo_toml_path)
            .unwrap();

        writeln!(cargo_toml, "\nwebbrowser = \"0.8.12\"").unwrap();

        // Compile the project for the selected target
        println!("Compiling for: {}", target);
        let status = Command::new("cargo")
            .current_dir(project_path)
            .args(["build", "--release", "--target", target])
            .status()
            .expect("Failed to compile the project");

        if status.success() {
            println!("Successfully compiled for target: {}", target);
        } else {
            eprintln!("Compilation failed for target: {}", target);
        }
      
        let executable_name = if target.contains("windows") {
            "github_opener.exe"
        } else {
            "github_opener"
        };
        let executable_path = project_path.join("target").join(target).join("release").join(&executable_name);

        if !executable_path.exists() {
            eprintln!("Expected executable does not exist: {:?}", executable_path);
            continue; // Skip this target and move to the next
        }
        
        let version = "1.0.0";
        let archive_name = format!("{}-{}-{}.zip", repo_name, version, target.replace("x86_64", "64bit").replace("i686", "32bit"));
        let archive_path = output_directory.join(&archive_name);

        // Ensure the output directory exists
        fs::create_dir_all(&output_directory).expect("Failed to create output directory");
        
        println!("Zipping to: {:?}", archive_path);
        
        create_zip_archive(
            &archive_path.to_str().unwrap(),
            vec![(&executable_name, &executable_path)]
        ).expect("Failed to create zip archive");

        println!("Compiled and zipped for target: {}", target);

        // The temporary directory and its contents are removed here when `dir` goes out of scope.
    }
}

fn is_target_installed(target: &str) -> bool {
    let output = Command::new("rustup")
        .args(["target", "list", "--installed"])
        .output()
        .expect("Failed to execute rustup command");

    if !output.status.success() {
        eprintln!("Failed to list installed targets");
        return false;
    }

    let installed_targets = std::str::from_utf8(&output.stdout).expect("Failed to read rustup output");
    installed_targets.lines().any(|line| line == target)
}

fn create_zip_archive(archive_path: &str, files: Vec<(&str, &Path)>) -> zip::result::ZipResult<()> {
    let file = File::create(archive_path)?;
    let mut zip = ZipWriter::new(file);

    let options = FileOptions::default()
        .compression_method(zip::CompressionMethod::Stored) // Change compression method if needed
        .unix_permissions(0o755);

    for (file_name, path) in files {
        zip.start_file(file_name, options)?;
        let mut f = File::open(path)?;
        std::io::copy(&mut f, &mut zip)?;
    }

    zip.finish()?;
    Ok(())
}
