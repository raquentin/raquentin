<h2>entitled.exe</h2>
<p>entitled-exe is a CLI tool to programmatically generate executables that open your repository on GitHub in the user's browser.</p>

<h3>How it works</h3>
<table style="border-collapse: collapse; border: none;">
<tr>
<td style="border: none;">
<img src="https://github.com/r4c3/entitled-exe/assets/63271957/bc50eee2-06b4-4ee8-bdf8-dcdbb02d9f05" alt="Relevant image" style="height: 340px; width: auto; display: block;">
</td>
<td style="border: none;">

- `cargo install entitled-exe`
- `cd <dir where you want /builds to go>`
- `entitled-exe`
- Enter repository name: `<user/repo>`
- Select compilation targets: `<use jkl; and space to select>`
- generating binaries...
- zipping binaries...
- zipped exes saved to /builds
- maintainer uploads /builds to GitHub releases
- script kiddie downloads .exe
- runs ./repo-name
- github page opens in browser
- cries

</td>
</tr>
</table>
