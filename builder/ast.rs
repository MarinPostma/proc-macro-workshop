DeriveInput {
    attrs: [],
    vis: Public(
        VisPublic {
            pub_token: Pub,
        },
    ),
    ident: Ident {
        ident: "Command",
        span: #0 bytes(1001..1008),
    },
    generics: Generics {
        lt_token: None,
        params: [],
        gt_token: None,
        where_clause: None,
    },
    data: Struct(
        DataStruct {
            struct_token: Struct,
            fields: Named(
                FieldsNamed {
                    brace_token: Brace,
                    named: [
                        Field {
                            attrs: [],
                            vis: Inherited,
                            ident: Some(
                                Ident {
                                    ident: "executable",
                                    span: #0 bytes(1015..1025),
                                },
                            ),
                            colon_token: Some(
                                Colon,
                            ),
                            ty: Path(
                                TypePath {
                                    qself: None,
                                    path: Path {
                                        leading_colon: None,
                                        segments: [
                                            PathSegment {
                                                ident: Ident {
                                                    ident: "String",
                                                    span: #0 bytes(1027..1033),
                                                },
                                                arguments: None,
                                            },
                                        ],
                                    },
                                },
                            ),
                        },
                        Comma,
                        Field {
                            attrs: [],
                            vis: Inherited,
                            ident: Some(
                                Ident {
                                    ident: "args",
                                    span: #0 bytes(1039..1043),
                                },
                            ),
                            colon_token: Some(
                                Colon,
                            ),
                            ty: Path(
                                TypePath {
                                    qself: None,
                                    path: Path {
                                        leading_colon: None,
                                        segments: [
                                            PathSegment {
                                                ident: Ident {
                                                    ident: "Vec",
                                                    span: #0 bytes(1045..1048),
                                                },
                                                arguments: AngleBracketed(
                                                    AngleBracketedGenericArguments {
                                                        colon2_token: None,
                                                        lt_token: Lt,
                                                        args: [
                                                            Type(
                                                                Path(
                                                                    TypePath {
                                                                        qself: None,
                                                                        path: Path {
                                                                            leading_colon: None,
                                                                            segments: [
                                                                                PathSegment {
                                                                                    ident: Ident {
                                                                                        ident: "String",
                                                                                        span: #0 bytes(1049..1055),
                                                                                    },
                                                                                    arguments: None,
                                                                                },
                                                                            ],
                                                                        },
                                                                    },
                                                                ),
                                                            ),
                                                        ],
                                                        gt_token: Gt,
                                                    },
                                                ),
                                            },
                                        ],
                                    },
                                },
                            ),
                        },
                        Comma,
                        Field {
                            attrs: [],
                            vis: Inherited,
                            ident: Some(
                                Ident {
                                    ident: "env",
                                    span: #0 bytes(1062..1065),
                                },
                            ),
                            colon_token: Some(
                                Colon,
                            ),
                            ty: Path(
                                TypePath {
                                    qself: None,
                                    path: Path {
                                        leading_colon: None,
                                        segments: [
                                            PathSegment {
                                                ident: Ident {
                                                    ident: "Vec",
                                                    span: #0 bytes(1067..1070),
                                                },
                                                arguments: AngleBracketed(
                                                    AngleBracketedGenericArguments {
                                                        colon2_token: None,
                                                        lt_token: Lt,
                                                        args: [
                                                            Type(
                                                                Path(
                                                                    TypePath {
                                                                        qself: None,
                                                                        path: Path {
                                                                            leading_colon: None,
                                                                            segments: [
                                                                                PathSegment {
                                                                                    ident: Ident {
                                                                                        ident: "String",
                                                                                        span: #0 bytes(1071..1077),
                                                                                    },
                                                                                    arguments: None,
                                                                                },
                                                                            ],
                                                                        },
                                                                    },
                                                                ),
                                                            ),
                                                        ],
                                                        gt_token: Gt,
                                                    },
                                                ),
                                            },
                                        ],
                                    },
                                },
                            ),
                        },
                        Comma,
                        Field {
                            attrs: [],
                            vis: Inherited,
                            ident: Some(
                                Ident {
                                    ident: "current_dir",
                                    span: #0 bytes(1084..1095),
                                },
                            ),
                            colon_token: Some(
                                Colon,
                            ),
                            ty: Path(
                                TypePath {
                                    qself: None,
                                    path: Path {
                                        leading_colon: None,
                                        segments: [
                                            PathSegment {
                                                ident: Ident {
                                                    ident: "String",
                                                    span: #0 bytes(1097..1103),
                                                },
                                                arguments: None,
                                            },
                                        ],
                                    },
                                },
                            ),
                        },
                        Comma,
                    ],
                },
            ),
            semi_token: None,
        },
    ),
}
