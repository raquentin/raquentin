use std::fs::{File, self};
use std::path::{PathBuf, Path};
use std::io::Write;
use std::process::Command;
use zip::write::{FileOptions, ZipWriter};

pub fn init_cargo_project(project_path: &Path, repo_name: &str) {
    Command::new("cargo")
        .arg("init")
        .arg("--name")
        .arg(repo_name)
        .arg(project_path.to_str().unwrap())
        .status()
        .expect("Failed to create temporary Cargo project");
}

pub fn write_main_rs(project_path: &Path, github_url: &str, sleep_duration: u64) {
    let source_code = format!(
        r#"use std::{{thread, time::Duration}};
        use webbrowser;
        
        fn main() {{
            thread::sleep(Duration::from_millis({}));
            webbrowser::open("{}").unwrap();
        }}"#,
        sleep_duration, github_url
    );

    let src_path = project_path.join("src");
    let main_rs = src_path.join("main.rs");
    fs::write(main_rs, source_code).expect("Failed to write main.rs");
}

pub fn add_dependencies_to_cargo_toml(project_path: &Path) {
    let cargo_toml_path = project_path.join("Cargo.toml");
    let mut cargo_toml = fs::OpenOptions::new()
        .append(true)
        .open(cargo_toml_path)
        .expect("Failed to open Cargo.toml");

    writeln!(cargo_toml, "\nwebbrowser = \"0.8\"").expect("Failed to write to Cargo.toml");
}

pub fn compile_project(project_path: &Path, target: &str) -> bool {
    let status = Command::new("cargo")
        .current_dir(project_path)
        .args(["build", "--release", "--target", target])
        .status()
        .expect("Failed to compile the project");

    status.success()
}

pub fn is_target_installed(target: &str) -> bool {
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

pub fn create_zip_archive(archive_path: &str, files: Vec<(&str, &Path)>) -> zip::result::ZipResult<()> {
    let file = File::create(archive_path)?;
    let mut zip = ZipWriter::new(file);

    let options = FileOptions::default()
        .compression_method(zip::CompressionMethod::Stored)
        .unix_permissions(0o755);

    for (file_name, path) in files {
        zip.start_file(file_name, options)?;
        let mut f = File::open(path)?;
        std::io::copy(&mut f, &mut zip)?;
    }

    zip.finish()?;
    Ok(())
}

pub fn package_executable(
    project_path: &Path,
    repo_name: &str,
    target: &str,
    version: &str,
    output_directory: &PathBuf,
) -> Result<(), String> {
    let executable_name = if target.contains("windows") {
        format!("{}.exe", repo_name)
    } else {
        repo_name.to_string()
    };

    let executable_path = project_path.join("target")
                                      .join(target)
                                      .join("release")
                                      .join(&executable_name);

    if !executable_path.exists() {
        return Err(format!("Expected executable does not exist: {:?}", executable_path));
    }

    let archive_name = format!(
        "{}-{}-{}.zip",
        repo_name,
        version,
        target,
    );
    let archive_path = output_directory.join(&archive_name);

    match create_zip_archive(
        &archive_path.to_str().unwrap(),
        vec![(&executable_name, &executable_path)],
    ) {
        Ok(_) => Ok(()),
        Err(e) => Err(format!("Failed to create zip archive: {}", e)),
    }
}
