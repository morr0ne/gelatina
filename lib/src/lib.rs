use itertools::Itertools;
use roxmltree::Document;
use std::{fmt::Display, str::FromStr};

/// A list of all keywords reserved by the language.
const KEYWORDS: [&str; 51] = [
    "as", "break", "const", "continue", "crate", "else", "enum", "extern", "false", "fn", "for",
    "if", "impl", "in", "let", "loop", "match", "mod", "move", "mut", "pub", "ref", "return",
    "self", "Self", "static", "struct", "super", "trait", "true", "type", "unsafe", "use", "where",
    "while", "async", "await", "dyn", "abstract", "become", "box", "do", "final", "macro",
    "override", "priv", "typeof", "unsized", "virtual", "yield", "try",
];

#[derive(Debug)]
pub enum Api {
    Gl,
    Gles1,
    Gles2,
    Glsc2,
}

impl FromStr for Api {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "gl" => Ok(Self::Gl),
            "gles1" => Ok(Self::Gles1),
            "gles2" => Ok(Self::Gles2),
            "glsc2" => Ok(Self::Glsc2),
            _ => Err("Invalid api"),
        }
    }
}

impl Api {
    pub const fn api(&self) -> &str {
        match self {
            Api::Gl => "gl",
            Api::Gles1 => "gles1",
            Api::Gles2 => "gles2",
            Api::Glsc2 => "glsc2",
        }
    }
}

pub struct GlRegistry {
    pub gl_enums: Vec<GlEnum>,
    pub gl_commands: Vec<GlCommand>,
    pub gl_features: Vec<GlFeature>,
    pub gl_extensions: Vec<GlExtension>,
}

pub struct GlEnum {
    pub name: String,
    pub value: String,
    pub bitmask: bool,
}

pub struct GlCommand {
    pub name: String,
    pub gl_params: Vec<GlParam>,
    pub return_type: String,
}

pub struct GlParam {
    pub gl_type: String,
    pub name: String,
}

pub struct GlFeature {
    pub api: Api,
}

pub struct GlExtension {}

#[derive(Debug, thiserror::Error)]
pub enum ParseError {
    #[error("")]
    Xml(#[from] roxmltree::Error),
}

impl GlRegistry {
    pub fn parse(xml: &str) -> Result<Self, ParseError> {
        let document = Document::parse(xml)?;

        let mut gl_enums = Vec::new();
        let mut gl_commands = Vec::new();
        let mut gl_features = Vec::new();
        let mut gl_extensions = Vec::new();

        for node in document.root().first_child().unwrap().children() {
            if node.is_element() {
                match node.tag_name().name() {
                    "enums" => {
                        if let Some(group) = node.attribute("group") {
                            // TODO: For some reasone nvidia used negative values here, no clue way. I'm fixing this another time.
                            if group == "TransformFeedbackTokenNV" {
                                continue;
                            }
                        }

                        let bitmask = if let Some(t) = node.attribute("type") {
                            t == "bitmask"
                        } else {
                            false
                        };

                        for gl_enum in node.children() {
                            if gl_enum.tag_name().name() == "enum" {
                                let name = gl_enum.attribute("name").unwrap().to_string();
                                let value = gl_enum.attribute("value").unwrap().to_string();

                                gl_enums.push(GlEnum {
                                    name,
                                    value,
                                    bitmask,
                                });
                            }
                        }
                    }
                    "commands" => {
                        for gl_command in node.children() {
                            if gl_command.tag_name().name() == "command" {
                                let mut name = None;
                                let mut gl_params = Vec::new();
                                let mut return_type = None;

                                for command_attr in gl_command.children() {
                                    match command_attr.tag_name().name() {
                                        "proto" => {
                                            name = Some(
                                                command_attr
                                                    .children()
                                                    .find(|node| node.tag_name().name() == "name")
                                                    .unwrap()
                                                    .text()
                                                    .unwrap()
                                                    .to_string(),
                                            );

                                            let mut gl_type = if let Some(ptype) = command_attr
                                                .children()
                                                .find(|node| node.tag_name().name() == "ptype")
                                            {
                                                ptype.text().unwrap().to_string()
                                            } else {
                                                "".to_string()
                                            };

                                            if let Some(text) = command_attr.text() {
                                                if let "const" = text.trim() {
                                                    gl_type = format!("*const {}", gl_type);
                                                }
                                            }

                                            if !gl_type.is_empty() {
                                                return_type = Some(format!("->{}", gl_type));
                                            } else {
                                                return_type = Some(gl_type)
                                            }
                                        }
                                        "param" => {
                                            let mut name = command_attr
                                                .children()
                                                .find(|node| node.tag_name().name() == "name")
                                                .unwrap()
                                                .text()
                                                .unwrap()
                                                .to_string();

                                            if KEYWORDS.contains(&name.as_str()) {
                                                name = format!("r#{}", name)
                                            }

                                            let gl_type = if let Some(node) = command_attr
                                                .children()
                                                .find(|node| node.tag_name().name() == "ptype")
                                            {
                                                let mut gl_type = match node.text().unwrap().trim()
                                                {
                                                    "struct _cl_context" => "*mut _cl_context",
                                                    "struct _cl_event" => "*mut _cl_event",
                                                    gl_type => gl_type,
                                                }
                                                .to_string();

                                                if let Some(tail) = node.tail() {
                                                    if tail.trim() == "*" {
                                                        if let Some(text) = command_attr.text() {
                                                            if let "const" = text.trim() {
                                                                gl_type =
                                                                    format!("*const {}", gl_type);
                                                            }
                                                        } else {
                                                            gl_type = format!("*mut {}", gl_type);
                                                        }
                                                    } else if tail.trim() == "*const*" {
                                                        gl_type =
                                                            format!("*const *const {}", gl_type);
                                                    }
                                                }

                                                gl_type
                                            } else {
                                                match command_attr.text().unwrap().trim() {
                                                    "const void *" => "*const c_void",
                                                    "const void **" | "const void *const*" => {
                                                        "*const *const c_void"
                                                    }
                                                    "void *" => "*mut c_void",
                                                    "void **" => "*mut *mut c_void",
                                                    text => panic!(
                                                        "Couldn't find a valid type\n {}",
                                                        text
                                                    ),
                                                }
                                                .to_string()
                                            };

                                            gl_params.push(GlParam { name, gl_type })
                                        }
                                        "alias" => {}
                                        "glx" => {}
                                        "vecequiv" => {}
                                        _ => {
                                            // dbg!(name);
                                        }
                                    }
                                }

                                gl_commands.push(GlCommand {
                                    name: name.unwrap(),
                                    gl_params,
                                    return_type: return_type.unwrap(),
                                });
                            }
                        }
                    }
                    "feature" => {}
                    _ => {}
                }
            }
        }

        Ok(Self {
            gl_enums,
            gl_commands,
            gl_features,
            gl_extensions,
        })
    }
}

impl Display for GlRegistry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let formated_enums = &self.gl_enums.iter().format_with("\n", |gl_enum, f| {
            // TODO: GL_ACTIVE_PROGRAM_EXT is defined multiple times. This a quick hack to generate at least the gl 4.6 api.
            if gl_enum.name == "GL_ACTIVE_PROGRAM_EXT" {
                f(&"")
            } else {
                let enum_type = if gl_enum.bitmask {
                    "GLbitfield"
                } else if gl_enum.value == "0xFFFFFFFFFFFFFFFF" {
                    "u64"
                } else {
                    "GLenum"
                };

                f(&format_args!(
                    "pub const {enum_name}: {enum_type} = {enum_value};",
                    enum_name = gl_enum.name,
                    enum_value = gl_enum.value
                ))
            }
        });

        let formated_fields = &self.gl_commands.iter().format_with(",\n", |gl_command, f| {
            f(&format_args!(
                r#"{command_name}: extern "system" fn({function_parameters}){function_return_type}"#,
                command_name=gl_command.name,
                function_parameters=&gl_command
                    .gl_params
                    .iter()
                    .format_with(",", |gl_param, f| f(&gl_param.gl_type)),
                function_return_type=gl_command.return_type
            ))
        });

        let formated_constructor = &self.gl_commands.iter().format_with(",\n", |gl_command, f| {
            f(&format_args!(
                // NOTE: Thise needs to be on multiple lines otherwise the formater fricks out. I'd probably be a good idea to report the bug.
                r#"{command_name}: transmute::<*const c_void, 
                extern "system" fn({function_parameters}){function_return_type}>
                (load_pointer(CStr::from_bytes_with_nul_unchecked(b"{command_name}\0"))?)"#,
                command_name = gl_command.name,
                function_parameters = &gl_command
                    .gl_params
                    .iter()
                    .format_with(",", |gl_param, f| f(&gl_param.gl_type)),
                function_return_type = gl_command.return_type,
            ))
        });

        let formated_methods = &self.gl_commands.iter().format_with("\n", |gl_command, f| {
            f(&format_args!(
                "pub unsafe fn {function_name}(&self,{function_parameters}){function_return_type}
                {{
                    (self.{inner_function})({inner_function_parameters})
                }}",
                function_name = gl_command.name.replacen("gl", "", 1),
                function_parameters =
                    &gl_command
                        .gl_params
                        .iter()
                        .format_with(",", |gl_param, f| f(&format_args!(
                            "{}:{}",
                            gl_param.name, gl_param.gl_type
                        ))),
                function_return_type = gl_command.return_type,
                inner_function = gl_command.name,
                inner_function_parameters = &gl_command
                    .gl_params
                    .iter()
                    .format_with(",", |gl_param, f| f(&gl_param.name)),
            ))
        });

        writeln!(
            f,
            r#"
#![allow(bad_style)]
#![allow(unused)]
#![allow(clippy::too_many_arguments)]
#![allow(clippy::missing_safety_doc)]
#![allow(clippy::upper_case_acronyms)]
use std::{{error::Error, ffi::CStr, fmt::Display, mem::transmute, os::raw::*}};

pub type Result<T, E = LoadError> = std::result::Result<T, E>;

#[derive(Debug)]
pub struct LoadError {{
    pub name: String,
    pub pointer: usize,
}}

impl Error for LoadError {{}}

impl Display for LoadError {{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {{
        write!(
            f,
            "Failed to load function \"{{}}\", expected a valid pointer instead got {{}}",
            self.name, self.pointer
        )
    }}
}}

pub use types::*;
pub mod types {{
    use std::os::raw::*;
    pub type GLvoid = c_void;
    pub type GLbyte = c_char;
    pub type GLubyte = c_uchar;
    pub type GLchar = c_char;
    pub type GLboolean = c_uchar;
    pub type GLshort = c_short;
    pub type GLushort = c_ushort;
    pub type GLint = c_int;
    pub type GLuint = c_uint;
    pub type GLint64 = i64;
    pub type GLuint64 = u64;
    pub type GLintptr = isize;
    pub type GLsizeiptr = isize;
    pub type GLintptrARB = isize;
    pub type GLsizeiptrARB = isize;
    pub type GLint64EXT = i64;
    pub type GLuint64EXT = u64;
    pub type GLsizei = GLint;
    pub type GLclampx = c_int;
    pub type GLfixed = GLint;
    pub type GLhalf = c_ushort;
    pub type GLhalfNV = c_ushort;
    pub type GLhalfARB = c_ushort;
    pub type GLenum = c_uint;
    pub type GLbitfield = c_uint;
    pub type GLfloat = c_float;
    pub type GLdouble = c_double;
    pub type GLclampf = c_float;
    pub type GLclampd = c_double;
    pub type GLcharARB = c_char;
    #[cfg(target_os = "macos")]
    pub type GLhandleARB = *const c_void;
    #[cfg(not(target_os = "macos"))]
    pub type GLhandleARB = c_uint;
    pub enum __GLsync {{}}
    pub type GLsync = *const __GLsync;
    pub enum _cl_context {{}}
    pub enum _cl_event {{}}
    pub type GLvdpauSurfaceNV = GLintptr;
    pub type GLeglClientBufferEXT = *const c_void;
    pub type GLeglImageOES = *const c_void;
    pub type GLDEBUGPROC = extern "system" fn(
        source: GLenum,
        type_: GLenum,
        id: GLuint,
        severity: GLenum,
        length: GLsizei,
        message: *const GLchar,
        userParam: *mut c_void,
    );
    pub type GLDEBUGPROCARB = extern "system" fn(
        source: GLenum,
        type_: GLenum,
        id: GLuint,
        severity: GLenum,
        length: GLsizei,
        message: *const GLchar,
        userParam: *mut c_void,
    );
    pub type GLDEBUGPROCKHR = extern "system" fn(
        source: GLenum,
        type_: GLenum,
        id: GLuint,
        severity: GLenum,
        length: GLsizei,
        message: *const GLchar,
        userParam: *mut GLvoid,
    );
    pub type GLDEBUGPROCAMD = extern "system" fn(
        id: GLuint,
        category: GLenum,
        severity: GLenum,
        length: GLsizei,
        message: *const GLchar,
        userParam: *mut GLvoid,
    );
    pub type GLVULKANPROCNV = extern "system" fn();
}}

pub use enums::*;
pub mod enums {{
use super::*;
pub const GL_ACTIVE_PROGRAM_EXT: GLenum = 0x8B8D;
{formated_enums}
}}

pub struct Gl {{
{formated_fields}
}}

impl Gl {{
    pub unsafe fn load<F>(mut loader_function: F) -> Result<Self>
    where
        F: FnMut(&CStr) -> *const c_void,
    {{
        let mut load_pointer = |name: &CStr| -> Result<*const c_void> {{
            let pointer = loader_function(name);
            let pointer_usize = pointer as usize;

            if pointer_usize == core::usize::MAX || pointer_usize < 8 {{
                Err(LoadError {{
                    name: name.to_string_lossy().to_string(),
                    pointer: pointer_usize,
                }})
            }} else {{
                Ok(pointer)
            }}
        }};

        Ok(Self {{
            {formated_constructor}
        }})
    }}

    {formated_methods}
}}"#
        )?;

        Ok(())
    }
}
