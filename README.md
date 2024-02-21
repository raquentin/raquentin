<h2>entitled.exe</h2>
<p>entitled-exe is a CLI tool to programmatically generate executables that open your repository on GitHub in the user's browser.</p>

[![Crates.io](https://img.shields.io/crates/v/entitled-exe.svg)](https://crates.io/crates/entitled-exe)
![Downloads](https://img.shields.io/crates/dv/entitled-exe/0.2.1)

### Quick Links
* [Installation](#installation)
* [crates.io/entitled-exe](https://crates.io/crates/entitled-exe)

### Usage
<table style="border-collapse: collapse; border: none;">
<tr>
<td style="border: none;">
<img src="https://github.com/r4c3/entitled-exe/assets/63271957/bc50eee2-06b4-4ee8-bdf8-dcdbb02d9f05" alt="Relevant image" style="height: 340px; width: auto; display: block;">
</td>
<td style="border: none;">

- do `cargo install entitled-exe`
- do `cd <dir where you want /builds to go>`
- run `entitled-exe`
- Enter repository name: `<user/repo>`
- Select compilation targets with `jk` and `space`
- generating binaries...
- zipping binaries...
- done!
- cd `builds`
- upload `builds` to GitHub releases
- script kiddie downloads `<repo>-X.Y.Z-<arch>.zip`
- extracts to `<repo>.exe` or just `<repo>` on unix
- runs `./<repo>`
- browser opens `https://github.com/<user>/<repo>`
- *cries*

</td>
</tr>
</table>

### Installation
You'll need Rust, Cargo, and rustup to install this. You'll also need the toolchains you choose to compile to; e.g. building for arm-linux-androidabi requires `rustup target add arm-linux-androideabi`.
After that, just install with cargo:
```
$ cargo install entitled-exe
```
