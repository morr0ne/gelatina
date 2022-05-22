use clap::Parser;
use color_eyre::Result;
use gelatina::{Api, GlRegistry};
use std::{fs, path::PathBuf};

/// A shitty generator
#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    /// Path to the gl.xml file, if not provided the latest one will be download from https://github.com/KhronosGroup/OpenGL-Registry/raw/main/xml/gl.xml
    #[clap(short, long)]
    registry: Option<PathBuf>,
    /// Which api to use. One of gl, gles1, gles2 or glsc2
    #[clap(short, long, default_value = "gl")]
    api: Api,
    /// Major version of the api
    #[clap(long, default_value = "4.6")]
    version: f32,
}

fn main() -> Result<()> {
    color_eyre::install()?;

    let Args { registry, .. } = Args::parse();

    let xml = if let Some(path) = registry {
        fs::read_to_string(path)?
    } else {
        reqwest::blocking::get(
            "https://github.com/KhronosGroup/OpenGL-Registry/raw/main/xml/gl.xml",
        )?
        .text()?
    };

    let gl_registry = GlRegistry::parse(&xml)?;

    print!("{gl_registry}");

    Ok(())
}
