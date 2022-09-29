// use std::sync::{Arc, Mutex};

use color_eyre::Report;
use postcard::experimental::schema::{NamedType, NamedVariant, Schema, SdmTy, Varint};
use tui::style::Style;
// use tui::style::Style;
use std::default::Default;
use tui_va_tree_edit::{Args, Branch, NumberType, StringType, Tree, TreeEdit, Value};

// macro_rules! type_from_default {
//     ($ty:expr; [$($ty_inner:expr,)?] $ty_value:ty) => {
//         $ty($($ty_inner.into(),)? Self::wrap_json(json!(<$ty_value>::default()))).into()
//     };
//     ($ty:expr; $ty_value:ty) => {
//         type_from_default!($ty; [] $ty_value)
//     };
//     ($ty:expr; $ty_inner:expr, $ty_value:ty) => {
//         type_from_default!($ty; [$ty_inner,] $ty_value)
//     };
// }

type Map<K, V> = linked_hash_map::LinkedHashMap<K, V>;
type List = Map<&'static str, Optional>;

pub use tui_va_tree_edit::Event as EventUI;
pub enum Event {
    UI(EventUI),
    GenerateMessage,
}
// pub type Value = Ref<serde_json::Value>;

// #[derive(Debug, Clone, Default)]
// pub struct Ref<T: ?Sized> {
//     inner: Arc<Mutex<T>>,
// }
// impl<T> Ref<T> {
//     pub fn new(value: T) -> Self {
//         Self {
//             inner: Arc::new(Mutex::new(value)),
//         }
//     }
//     pub fn as_ref(&mut self) -> &mut Arc<Mutex<T>> {
//         &mut self.inner
//     }
//     pub fn lock(&self) -> std::sync::LockResult<std::sync::MutexGuard<'_, T>> {
//         self.inner.lock()
//     }
// }
// impl<T> From<T> for Ref<T> {
//     fn from(value: T) -> Self {
//         Self::new(value)
//     }
// }

// #[derive(Debug, Clone, PartialEq)]
// pub enum Struct {
//     Struct,
//     Enum,
//     EmunIn(&'static str),
// }
// impl Struct {
//     pub fn is_struct(&self) -> bool {
//         matches!(self, Self::Struct)
//     }
//     pub fn is_enum(&self) -> bool {
//         matches!(self, Self::Enum | Self::EmunIn(_))
//     }
// }

// #[derive(Debug, Clone)]
// pub enum NumberArg {
//     U8,
//     I8,
//     U16,
//     I16,
//     U32,
//     I32,
//     U64,
//     I64,
//     F32,
//     F64,
//     Usize,
//     Isize,
// }

// #[derive(Debug, Clone)]
// pub enum StringArg {
//     Char,
//     String,
// }

#[derive(Clone)]
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
pub struct TuiProtoc<'a, T: Schema + for<'de> serde::de::Deserialize<'de> + 'a> {
    ui: TreeEdit<'a>,
    offset: Vec<&'static str>,
    ty: Type,
    phantom: std::marker::PhantomData<&'a T>,
}
impl<'a, T: Schema + for<'de> serde::de::Deserialize<'de> + 'a> AsRef<TuiProtoc<'a, T>>
    for TuiProtoc<'a, T>
{
    fn as_ref(&self) -> &TuiProtoc<'a, T> {
        self
    }
}
impl<'a, T: Schema + for<'de> serde::de::Deserialize<'de> + 'a> TuiProtoc<'a, T> {
    pub fn new(title: impl Into<String>, offset: Vec<&'static str>) -> Self {
        let ty = Optional::from(T::SCHEMA).unwrap();
        Self {
            ui: Self::generate_ui(title.into(), Self::incise_ui(&ty, offset.iter()).unwrap()),
            offset,
            ty,
            phantom: Default::default(),
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

    pub fn transition(&mut self, event: Event) {
        match event {
            Event::UI(event) => self.ui.transition(event),
            Event::GenerateMessage => todo!(),
        }
    }

    /*fn unwrap(src: &NamedType) -> Optional {
        use serde_json::json;
        <Vec<usize> as Default>::default();

        match src.ty {
            SdmTy::Struct(values) => {
                let mut list = values
                    .iter()
                    .map(|&value| (value.name, Self::unwrap(value.ty)))
                    .peekable();

                if list.len() == 1 && {
                    list.peek().map_or(false, |(_, opt)| {
                        opt.is_false() && opt.unwrap_ref().is_enum()
                    })
                } {
                    let (name, en) = list
                        .next()
                        .map(|(name, opt)| (name, opt.unwrap().as_list().unwrap()))
                        .unwrap();

                    Self::List(Lists::EmunIn(name), en).into()
                } else {
                    Self::List(Lists::List, list.collect::<List>()).into()
                }
            }
            SdmTy::Enum(values) => Self::List(
                Lists::Enum,
                values
                    .iter()
                    .map(|&&NamedVariant { name, ty }| {
                        (name, Self::unwrap(&NamedType { name, ty }))
                    })
                    .collect::<List>(),
            )
            .into(),
            SdmTy::Unit => Self::None.into(),
            SdmTy::Bool => type_from_default!(Self::Bool; bool),
            SdmTy::I8 => type_from_default!(Self::Number; NumberArg::I8, i8),
            SdmTy::U8 => type_from_default!(Self::Number; NumberArg::U8, u8),
            SdmTy::Varint(ty) => match ty {
                Varint::I16 => type_from_default!(Self::Number; NumberArg::I16, i16),
                Varint::U16 => type_from_default!(Self::Number; NumberArg::U16, u16),
                Varint::I32 => type_from_default!(Self::Number; NumberArg::I32, i32),
                Varint::U32 => type_from_default!(Self::Number; NumberArg::U32, u32),
                Varint::I64 | Varint::I128 => type_from_default!(Self::Number; NumberArg::I64, i16),
                Varint::U64 | Varint::U128 => type_from_default!(Self::Number; NumberArg::U64, i32),
                Varint::Isize => type_from_default!(Self::Number; NumberArg::Isize, isize),
                Varint::Usize => type_from_default!(Self::Number; NumberArg::Usize, usize),
            },
            SdmTy::F32 => type_from_default!(Self::Number; NumberArg::F32, f32),
            SdmTy::F64 => type_from_default!(Self::Number; NumberArg::F64, f64),
            SdmTy::Char => type_from_default!(Self::String; StringArg::Char, String),
            SdmTy::String => type_from_default!(Self::String; StringArg::String, String),
            SdmTy::Option(value) => {
                let unwrapped = Self::unwrap(value).unwrap();
                if let Self::List(Lists::Enum | Lists::EmunIn(_), _) = unwrapped {
                    unwrapped.into()
                } else {
                    Optional::True(unwrapped)
                }
            }
            SdmTy::Seq(value) => {
                type_from_default!(Self::Array; Self::unwrap(value).unwrap(), Vec::<serde_json::Value>)
            }
            SdmTy::TupleVariant(value) => Self::unwrap(value[0]),
            SdmTy::ByteArray => todo!(),
            SdmTy::UnitStruct => todo!(),
            SdmTy::UnitVariant => todo!(),
            SdmTy::NewtypeStruct(_) => todo!(),
            SdmTy::NewtypeVariant(_) => todo!(),
            SdmTy::Tuple(_) => todo!(),
            SdmTy::TupleStruct(_) => todo!(),
            SdmTy::Map { key: _, val: _ } => todo!(),
            SdmTy::StructVariant(_) => todo!(),
        }
    }*/

    // fn wrap_json(value: serde_json::Value) -> Value {
    //     value.into()
    // }

    // pub fn as_ref(&self) -> &Self {
    //     self
    // }

    // pub fn is_enum(&self) -> bool {
    //     matches!(self, Self::List(Lists::Enum | Lists::EmunIn(_), _))
    // }
    // pub fn is_list(&self) -> bool {
    //     matches!(self, Self::List(Lists::List, _))
    // }
    // pub fn as_list(self) -> Option<List> {
    //     if let Self::List(_, en) = self {
    //         Some(en)
    //     } else {
    //         None
    //     }
    // }
    fn generate_ui(title: String, ty: &Type) -> TreeEdit<'a> {
        let mut ret = TreeEdit::new(title);

        if ty.is_enum() {
            for branch in Self::generate_ui_enum(ty.as_enum_ref().unwrap().1).into_iter() {
                ret = ret.tab(branch);
            }
        }

        ret
    }
    fn generate_ui_enum(list: &List) -> Vec<Branch<'a>> {
        let mut ret = Vec::default();
        list.iter().for_each(|(&name, opt)| {
            if let Type::Enum(_, list) = opt.unwrap_ref() {
                ret.push(
                    Self::generate_ui_enum(list)
                        .into_iter()
                        .fold(Tree::new(name), |tree, branch| tree.branch(branch))
                        .into(),
                );
            } else if let Type::Struct(list) = opt.unwrap_ref() {
                ret.push(Self::generate_ui_struct(name, list).into());
            } else if !opt.unwrap_ref().is_none() {
                ret.push(Self::generate_ui_struct(name, &{
                    let mut list = List::new();
                    list.insert("Value", opt.clone());
                    list
                }).into())
            } else {
                ret.push(Tree::new(name).into())
            }
        });

        ret
    }
    fn generate_ui_struct(name: &'static str, list: &List) -> Args<'a> {
        list.iter().fold(
            Args::new(name)
                .names(list.iter().fold(vec![], |mut vec, (&name, _)| {
                    vec.push(name);
                    vec
                }))
                .columns(list.iter().fold(vec!["Value"], |mut vec, (_, opt)| {
                    if vec.len() == 1 && opt.is_true() {
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
                    .fold(Tree::new(""), |tree, branch| tree.branch(branch))
                    .into(),
            ),
            Type::Struct(list) => Some(Self::generate_ui_struct("", list).into()),
        }
    }

    fn incise_ui<'b>(
        value: &'b Type,
        mut key: std::slice::Iter<&'static str>,
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
            .or(value.as_enum_ref().and_then(|(_, list)| Some(list)))
        {
            let name = key.next();

            if name == None {
                return Ok(value);
            } else if let Some(value) = list.get(name.unwrap()) {
                return Self::incise_ui(value.unwrap_ref(), key);
            }
        }

        Err(Report::msg("Such a sequence does not exist"))
    }

    // pub fn generate(&self, position: Vec<&'static str>) -> Result<T, String> {
    //     let generated = Self::generate_impl(self, position.iter().peekable());
    //     let msg = format!("Msg: {};", generated);
    //     serde_json::from_value(generated).map_err(|err| format!("Err: {err}; {msg}"))
    // }

    // fn generate_impl(
    //     value: &TreeEdit,
    //     mut key: std::iter::Peekable<std::slice::Iter<'_, &str>>,
    // ) -> serde_json::Value {
    //     match value {
    //         Type::None => serde_json::Value::Null,
    //         Type::Bool(value)
    //         | Type::Number(_, value)
    //         | Type::String(_, value)
    //         | Type::Array(_, value) => value.lock().unwrap().clone(),
    //         Type::List(ty, list) => {
    //             if let Lists::EmunIn(_) = ty {
    //                 key.next();
    //             }

    //             let map = if let Lists::List = ty {
    //                 key.next();
    //                 list.iter()
    //                     .map(|(&name, opt)| {
    //                         (
    //                             name.to_string(),
    //                             Self::generate_impl(opt.unwrap_ref(), key.clone()),
    //                         )
    //                     })
    //                     .collect()
    //             } else {
    //                 list.iter()
    //                     .find(|(&name, _)| name == **key.peek().unwrap_or(&&""))
    //                     .map(|(&name, opt)| {
    //                         key.next();
    //                         (name.to_string(), Self::generate_impl(opt.unwrap_ref(), key))
    //                     })
    //                     .map_or(serde_json::Map::new(), |(key, value)| {
    //                         let mut map = serde_json::Map::new();
    //                         map.insert(key, value);
    //                         map
    //                     })
    //             };

    //             let ret = serde_json::Value::Object(if let Lists::EmunIn(name) = ty {
    //                 let mut _map = serde_json::Map::new();
    //                 _map.insert((*name).into(), serde_json::Value::Object(map));
    //                 _map
    //             } else {
    //                 map
    //             });

    //             ret
    //         }
    //     }
    // }
}
// impl From<Type> for Value {
//     fn from(value: Type) -> Self {
//         match value {
//             Type::None => Type::wrap_json(serde_json::Value::Null),
//             Type::Bool(value)
//             | Type::Number(_, value)
//             | Type::String(_, value)
//             | Type::Array(_, value) => value,
//             Type::List(ty, list) => {
//                 let mut map = serde_json::Map::new();

//                 for (&name, value) in &list {
//                     map.insert(name.into(), value.unwrap_ref().into());
//                 }

//                 let ret = serde_json::Value::Object(map);

//                 Type::wrap_json(if let Lists::EmunIn(name) = ty {
//                     let mut map = serde_json::Map::new();
//                     map.insert(name.into(), ret);
//                     serde_json::Value::Object(map)
//                 } else {
//                     ret
//                 })
//             }
//         }
//     }
// }
// impl From<&Type> for serde_json::Value {
//     fn from(value: &Type) -> Self {
//         match value {
//             Type::None => serde_json::Value::Null,
//             Type::Bool(value)
//             | Type::Number(_, value)
//             | Type::String(_, value)
//             | Type::Array(_, value) => value.lock().unwrap().clone(),
//             Type::List(ty, list) => {
//                 let mut map = serde_json::Map::new();

//                 for (&name, value) in list {
//                     map.insert(name.into(), value.unwrap_ref().into());
//                 }

//                 let ret = serde_json::Value::Object(map);

//                 if let Lists::EmunIn(name) = ty {
//                     let mut map = serde_json::Map::new();
//                     map.insert((*name).into(), ret);
//                     serde_json::Value::Object(map)
//                 } else {
//                     ret
//                 }
//             }
//         }
//     }
// }

#[derive(Clone)]
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
