use color_eyre::Report;
use postcard::experimental::schema::{NamedType, NamedVariant, Schema, SdmTy, Varint};
use std::{collections::VecDeque, default::Default, fmt::Debug};
use tui::style::Style;
use tui_va_tree_edit::{
    Args, Branch, Branches, Node, NumberType, StringType, Tree, TreeEdit, Type as TreeType, Value,
};

type Map<K, V> = linked_hash_map::LinkedHashMap<K, V>;
type List = Map<&'static str, Optional>;

pub use tui_va_tree_edit::Event as EventUI;
pub enum Event {
    UI(EventUI),
    GenerateMessage,
}

#[derive(Debug, Clone)]
pub enum Type {
    None,
    Bool,
    Number(NumberType),
    String(StringType),
    Array(Box<Type>),
    Enum(Option<&'static str>, List),
    Struct(List),
}
impl AsRef<Type> for Type {
    fn as_ref(&self) -> &Type {
        self
    }
}
impl Type {
    pub fn is_none(&self) -> bool {
        matches!(self, Self::None)
    }
    pub fn is_enum(&self) -> bool {
        matches!(self, Self::Enum(_, _))
    }
    pub fn is_struct(&self) -> bool {
        matches!(self, Self::Struct(_))
    }
    pub fn as_struct(self) -> Option<List> {
        if let Self::Struct(en) = self {
            Some(en)
        } else {
            None
        }
    }
    pub fn as_struct_ref(&self) -> Option<&List> {
        if let Self::Struct(en) = self {
            Some(en)
        } else {
            None
        }
    }
    pub fn as_enum(self) -> Option<(Option<&'static str>, List)> {
        if let Self::Enum(opt, en) = self {
            Some((opt, en))
        } else {
            None
        }
    }
    pub fn as_enum_ref(&self) -> Option<(&Option<&'static str>, &List)> {
        if let Self::Enum(opt, en) = self {
            Some((opt, en))
        } else {
            None
        }
    }
}

#[derive(Clone)]
pub struct TuiProtoc<'a, T: Schema + for<'de> serde::de::Deserialize<'de> + 'static> {
    ui: TreeEdit<'a>,
    offset: Vec<&'static str>,
    ty: Type,
    queue: VecDeque<T>,
}
impl<'a, T: Schema + for<'de> serde::de::Deserialize<'de>> AsRef<TuiProtoc<'a, T>>
    for TuiProtoc<'a, T>
{
    fn as_ref(&self) -> &TuiProtoc<'a, T> {
        self
    }
}
impl<'a, T: Schema + for<'de> serde::de::Deserialize<'de> + Debug + 'static> TuiProtoc<'a, T> {
    pub fn new(title: impl Into<String>, offset: Vec<&'static str>) -> Self {
        let ty = Optional::from(T::SCHEMA).unwrap();
        Self {
            ui: Self::generate_ui(title.into(), Self::incise_ui(&ty, offset.iter()).unwrap()),
            offset,
            ty,
            queue: Default::default(),
        }
    }

    pub fn style(mut self, style: Style) -> Self {
        self.ui = self.ui.style(style);
        self
    }
    pub fn highlight_style(mut self, style: Style) -> Self {
        self.ui = self.ui.highlight_style(style);
        self
    }

    pub fn ui(&self) -> &TreeEdit<'a> {
        &self.ui
    }

    pub fn pool(&mut self) -> Option<T> {
        self.queue.pop_front()
    }

    pub fn transition(&mut self, event: Event) {
        match event {
            Event::UI(event) => self.ui.transition(event),
            Event::GenerateMessage => {
                if !self.ui.in_input_mode() {
                    self.generate_msg(self.ui.position()).map_or_else(
                        |str| log::error!(target:"tui_protoc", "{str}"),
                        |msg| self.queue.push_back(msg),
                    )
                }
            }
        }
    }

    fn generate_ui(title: String, ty: &Type) -> TreeEdit<'a> {
        let mut ret = TreeEdit::new(title);

        if ty.is_enum() {
            for (name, branch) in Self::generate_ui_enum(ty.as_enum_ref().unwrap().1).into_iter() {
                ret = ret.tab(name, branch);
            }
        }

        ret
    }
    fn generate_ui_enum(list: &List) -> Branches<'a> {
        list.iter()
            .fold(Branches::default(), |mut ret, (&name, opt)| {
                if let Type::Enum(_, list) = opt.unwrap_ref() {
                    ret.insert(
                        name.into(),
                        Self::generate_ui_enum(list)
                            .into_iter()
                            .fold(Tree::default(), |tree, (name, branch)| {
                                tree.branch(name, branch)
                            })
                            .into(),
                    );
                } else if let Type::Struct(list) = opt.unwrap_ref() {
                    ret.insert(name.into(), Self::generate_ui_struct(list).into());
                } else if !opt.unwrap_ref().is_none() {
                    ret.insert(
                        name.into(),
                        Self::generate_ui_struct(&[(">>", opt.clone())].into_iter().collect())
                            .into(),
                    );
                } else {
                    ret.insert(name.into(), Tree::default().into());
                }
                ret
            })
    }
    fn generate_ui_struct(list: &List) -> Args<'a> {
        list.iter().fold(
            Args::default()
                .names(list.iter().fold(vec![], |mut vec, (&name, _)| {
                    vec.push(name);
                    vec
                }))
                .columns(list.iter().fold(vec!["Value"], |mut vec, (_, opt)| {
                    if !vec.contains(&"State") && opt.is_true() {
                        vec.push("State");
                    }
                    vec
                })),
            |mut args, (&name, opt)| {
                if let Some(value) = Self::generate_ui_value(opt.unwrap_ref()) {
                    if opt.is_true() {
                        args = args.value(name, "State", true)
                    }
                    args.value(name, "Value", value)
                } else {
                    args
                }
            },
        )
    }
    fn generate_ui_value(ty: &Type) -> Option<Value<'a>> {
        match ty {
            Type::None => None,
            Type::Bool => Some(false.into()),
            Type::Number(ty) => Some(match ty {
                NumberType::U8 => u8::default().into(),
                NumberType::I8 => i8::default().into(),
                NumberType::U16 => u16::default().into(),
                NumberType::I16 => i16::default().into(),
                NumberType::U32 => u32::default().into(),
                NumberType::I32 => i32::default().into(),
                NumberType::U64 => u64::default().into(),
                NumberType::I64 => i64::default().into(),
                NumberType::F32 => f32::default().into(),
                NumberType::F64 => f64::default().into(),
                NumberType::Usize => usize::default().into(),
                NumberType::Isize => isize::default().into(),
            }),
            Type::String(_) => Some(String::default().into()),
            Type::Array(ty) => {
                Self::generate_ui_value(ty.as_ref()).map(|value| value.into_clear_array())
            }
            Type::Enum(_, list) => Some(
                Self::generate_ui_enum(list)
                    .into_iter()
                    .fold(Tree::default(), |tree, (name, branch)| {
                        tree.branch(name, branch)
                    })
                    .into(),
            ),
            Type::Struct(list) => Some(Self::generate_ui_struct(list).into()),
        }
    }

    fn incise_ui<'b>(
        value: &'b Type,
        mut key: impl Iterator<Item = &'b &'b str> + Debug,
    ) -> Result<&'b Type, Report> {
        if let Some(err) = value.as_ref().as_enum_ref().and_then(|(ty, _)| {
            ty.and_then(|name| {
                if name != *key.next().unwrap_or(&name) {
                    Some(Report::msg(format!(
                        "Such a sequence does not exist {:?} {:?}",
                        key, name
                    )))
                } else {
                    None
                }
            })
        }) {
            return Err(err);
        } else if let Some(list) = value
            .as_ref()
            .as_struct_ref()
            .or_else(|| value.as_enum_ref().map(|(_, list)| list))
        {
            if let Some(res) = key.next().map_or(Some(Ok(value)), |&name| {
                list.get(name)
                    .map(|value| Self::incise_ui(value.unwrap_ref(), key))
            }) {
                return res;
            }
        }

        Err(Report::msg("Such a sequence does not exist"))
    }

    pub fn generate_msg(&self, position: &[Node]) -> Result<T, String> {
        let generated = self
            .ui
            .get_current_tab()
            .map(|(name, branch)| {
                Self::generate_msg_offset(
                    &self.ty,
                    branch,
                    self.offset.iter().chain([name.as_str()].iter()),
                    position.iter().skip(1),
                )
            })
            .unwrap_or_default();

        let msg = format!("Msg: {};", generated);
        serde_json::from_value(generated).map_err(|err| format!("Err: {err}; {msg}"))
    }

    fn generate_msg_offset<'b>(
        ty: &Type,
        value: &Branch<'a>,
        mut key: impl Iterator<Item = &'b &'b str> + Clone,
        key_ui: impl Iterator<Item = &'b Node> + Clone,
    ) -> serde_json::Value {
        use serde_json::json;
        match ty {
            Type::None => serde_json::Value::Null,
            Type::Bool => json!(bool::default()),
            Type::Number(ty) => match ty {
                NumberType::F32 | NumberType::F64 => json!(f32::default()),
                _ => serde_json::Value::Number(0.into()),
            },
            Type::String(_) => serde_json::Value::String("".into()),
            Type::Array(_) => serde_json::Value::Array(vec![]),
            ty => {
                if let Some(nkey) = key.next() {
                    if let Type::Enum(en, list) = ty {
                        if en.is_some() { key.next() } else { Some(nkey) }
                            .and_then(|&nkey| {
                                list.iter()
                                    .find_map(|(&name, opt)| {
                                        if name == nkey {
                                            Some(opt.unwrap_ref())
                                        } else {
                                            None
                                        }
                                    })
                                    .map(|ty| {
                                        serde_json::Value::Object(
                                            [(
                                                nkey.to_string(),
                                                Self::generate_msg_offset(ty, value, key, key_ui),
                                            )]
                                            .into_iter()
                                            .collect::<serde_json::Map<_, _>>(),
                                        )
                                    })
                            })
                            .map_or(serde_json::Value::Null, |value| {
                                if let Some(name) = en {
                                    serde_json::Value::Object(
                                        [(name.to_string(), value)]
                                            .into_iter()
                                            .collect::<serde_json::Map<_, _>>(),
                                    )
                                } else {
                                    value
                                }
                            })
                    } else if let Type::Struct(list) = ty {
                        serde_json::Value::Object(
                            list.into_iter()
                                .map(|(&name, opt)| {
                                    (
                                        name.to_string(),
                                        Self::generate_msg_offset(
                                            opt.unwrap_ref(),
                                            value,
                                            key.clone(),
                                            key_ui.clone(),
                                        ),
                                    )
                                })
                                .collect::<serde_json::Map<_, _>>(),
                        )
                    } else {
                        serde_json::Value::Null
                    }
                } else {
                    Self::generate_msg_ui(ty, value, key_ui)
                }
            }
        }
    }

    fn generate_msg_ui<'b>(
        ty: &Type,
        value: &Branch<'a>,
        mut key: impl Iterator<Item = &'b Node> + Clone,
    ) -> serde_json::Value {
        match value {
            Branch::Args(args) => {
                if let Some(value) = {
                    let names = args.get_names_raw();
                    if names.len() == 1 && names.contains(&">>".to_string()) {
                        args.get_value_by_indexes(0, 0)
                    } else {
                        None
                    }
                } {
                    return Self::tree_value_to_serde_value(ty, value, key);
                } else {
                    ty.as_struct_ref().map(|list| {
                        key.next();
                        list.into_iter()
                            .filter_map(|(&name, opt)| {
                                opt.as_true()
                                    .and_then(|_| {
                                        args.get_value(name, "State").and_then(|value| {
                                            value.as_bool().and_then(|state| {
                                                if !*state {
                                                    Some(())
                                                } else {
                                                    None
                                                }
                                            })
                                        })
                                    })
                                    .map_or(Some(opt.unwrap_ref()), |_| None)
                                    .and_then(|ty| {
                                        args.get_value_by_cindex(name, 0)
                                            .map(|value| (name, ty, value))
                                    })
                            })
                            .fold(serde_json::Map::new(), |mut map, (name, ty, value)| {
                                map.insert(
                                    name.to_string(),
                                    Self::tree_value_to_serde_value(ty, value, key.clone()),
                                );
                                map
                            })
                    })
                }
            }
            Branch::Tree(tree) => {
                if !tree.get_branches().is_empty() {
                    key.next()
                        .map(|node| node.text())
                        .and_then(|text| {
                            Some(text).zip(
                                tree.get_branches()
                                    .iter()
                                    .find_map(
                                        |(name, branch)| {
                                            if name == text {
                                                Some(branch)
                                            } else {
                                                None
                                            }
                                        },
                                    )
                                    .zip(ty.as_enum_ref().and_then(|(_, list)| {
                                        list.iter().find_map(|(name, opt)| {
                                            if name.eq(text) {
                                                Some(opt.unwrap_ref())
                                            } else {
                                                None
                                            }
                                        })
                                    })),
                            )
                        })
                        .map(|(name, (value, ty))| {
                            [(name.clone(), Self::generate_msg_ui(ty, value, key))]
                                .into_iter()
                                .collect()
                        })
                } else {
                    return serde_json::Value::Null;
                }
            }
        }
        .map_or(
            serde_json::Value::Object(serde_json::Map::default()),
            |map| {
                serde_json::Value::Object(if let Type::Enum(Some(name), _) = ty {
                    [(name.to_string(), serde_json::Value::Object(map))]
                        .into_iter()
                        .collect()
                } else {
                    map
                })
            },
        )
    }

    fn tree_value_to_serde_value<'b>(
        ty: &Type,
        value: &Value<'a>,
        key: impl Iterator<Item = &'b Node> + Clone,
    ) -> serde_json::Value {
        use serde_json::json;
        match value.get_type() {
            TreeType::None => serde_json::Value::Null,
            TreeType::Bool => serde_json::Value::Bool(*value.as_bool().unwrap()),
            TreeType::Number(ty) => match ty {
                NumberType::U8 => json!(value.parse::<u8>().unwrap_or_default()),
                NumberType::I8 => json!(value.parse::<i8>().unwrap_or_default()),
                NumberType::U16 => json!(value.parse::<u16>().unwrap_or_default()),
                NumberType::I16 => json!(value.parse::<i16>().unwrap_or_default()),
                NumberType::U32 => json!(value.parse::<u32>().unwrap_or_default()),
                NumberType::I32 => json!(value.parse::<i32>().unwrap_or_default()),
                NumberType::U64 => json!(value.parse::<u64>().unwrap_or_default()),
                NumberType::I64 => json!(value.parse::<i64>().unwrap_or_default()),
                NumberType::F32 => json!(value.parse::<f32>().unwrap_or_default()),
                NumberType::F64 => json!(value.parse::<f64>().unwrap_or_default()),
                NumberType::Usize => json!(value.parse::<usize>().unwrap_or_default()),
                NumberType::Isize => json!(value.parse::<isize>().unwrap_or_default()),
            },
            TreeType::String(ty) => match ty {
                StringType::Char => value.parse::<String>().and_then(|str| {
                    if !str.is_empty() {
                        str.as_bytes()
                            .first()
                            .map(|byte| *byte as char)
                            .map(|ch| ch.to_string())
                    } else {
                        None
                    }
                }),
                StringType::String => value.parse::<String>(),
            }
            .unwrap_or_default()
            .into(),
            TreeType::Array(_) => serde_json::Value::Array(vec![]),
            TreeType::Struct => Self::generate_msg_ui(ty, value.as_struct().unwrap(), key),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Optional {
    True(Type),
    False(Type),
}
impl Optional {
    pub fn unwrap(self) -> Type {
        match self {
            Self::True(value) | Self::False(value) => value,
        }
    }
    pub fn unwrap_ref(&self) -> &Type {
        match self {
            Self::True(value) | Self::False(value) => value,
        }
    }
    pub fn unwrap_mut_ref(&mut self) -> &mut Type {
        match self {
            Self::True(value) | Self::False(value) => value,
        }
    }

    pub fn as_true(&self) -> Option<&Type> {
        if self.is_true() {
            Some(self.unwrap_ref())
        } else {
            None
        }
    }
    pub fn as_false(&self) -> Option<&Type> {
        if self.is_false() {
            Some(self.unwrap_ref())
        } else {
            None
        }
    }

    pub fn is_true(&self) -> bool {
        matches!(self, Self::True(_))
    }
    pub fn is_false(&self) -> bool {
        matches!(self, Self::False(_))
    }
}
impl From<Type> for Optional {
    fn from(value: Type) -> Self {
        Self::False(value)
    }
}
impl From<&NamedType> for Optional {
    fn from(src: &NamedType) -> Self {
        match src.ty {
            SdmTy::Struct(values) => {
                let mut list = values
                    .iter()
                    .map(|&value| (value.name, value.ty.into()))
                    .peekable();

                if list.len() == 1 && {
                    list.peek().map_or(false, |(_, opt): &(&str, Optional)| {
                        opt.is_false() && opt.unwrap_ref().is_enum()
                    })
                } {
                    let (name, en) = list
                        .next()
                        .map(|(name, opt)| (name, opt.unwrap().as_enum().unwrap().1))
                        .unwrap();

                    Type::Enum(Some(name), en).into()
                } else {
                    Type::Struct(list.collect::<List>()).into()
                }
            }
            SdmTy::Enum(values) => Type::Enum(
                None,
                values
                    .iter()
                    .map(|&&NamedVariant { name, ty }| (name, (&NamedType { name, ty }).into()))
                    .collect::<List>(),
            )
            .into(),
            SdmTy::Unit => Type::None.into(),
            SdmTy::Bool => Type::Bool.into(),
            SdmTy::I8 => Type::Number(NumberType::I8).into(),
            SdmTy::U8 => Type::Number(NumberType::U8).into(),
            SdmTy::Varint(ty) => match ty {
                Varint::I16 => Type::Number(NumberType::I16).into(),
                Varint::U16 => Type::Number(NumberType::U16).into(),
                Varint::I32 => Type::Number(NumberType::I32).into(),
                Varint::U32 => Type::Number(NumberType::U32).into(),
                Varint::I64 | Varint::I128 => Type::Number(NumberType::I64).into(),
                Varint::U64 | Varint::U128 => Type::Number(NumberType::U64).into(),
                Varint::Isize => Type::Number(NumberType::Isize).into(),
                Varint::Usize => Type::Number(NumberType::Usize).into(),
            },
            SdmTy::F32 => Type::Number(NumberType::F32).into(),
            SdmTy::F64 => Type::Number(NumberType::F64).into(),
            SdmTy::Char => Type::String(StringType::Char).into(),
            SdmTy::String => Type::String(StringType::String).into(),
            SdmTy::ByteArray => todo!(),
            SdmTy::Option(value) => {
                let unwrapped = Self::from(*value).unwrap();
                if unwrapped.is_enum() {
                    unwrapped.into()
                } else {
                    Optional::True(unwrapped)
                }
            }
            SdmTy::Seq(value) => Type::Array(Box::new(Self::from(*value).unwrap())).into(),
            SdmTy::TupleVariant(value) => Self::from(value[0]),
            SdmTy::UnitStruct => todo!(),
            SdmTy::UnitVariant => todo!(),
            SdmTy::NewtypeStruct(_) => todo!(),
            SdmTy::NewtypeVariant(_) => todo!(),
            SdmTy::Tuple(_) => todo!(),
            SdmTy::TupleStruct(_) => todo!(),
            SdmTy::Map { .. } => todo!(),
            SdmTy::StructVariant(_) => todo!(),
        }
    }
}
