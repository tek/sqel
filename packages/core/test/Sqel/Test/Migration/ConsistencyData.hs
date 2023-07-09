module Sqel.Test.Migration.ConsistencyData where

import Exon (exon)

meta1 :: ByteString
meta1 =
  [exon|[
  {
    "name": {
      "tag": "table",
      "value": "dat"
    },
    "statementsMigration": [
      "alter table \"dat\" drop column \"old\"",
      "alter table \"dat\" add column \"size\" bigint",
      "update dat set \"size\" = 0",
      "alter table \"dat\" alter column \"size\" set not null",
      "alter table \"dat\" add column \"pord\" \"sqel_type__pord\"",
      "alter table \"dat\" alter column \"pord\" set not null",
      "create type \"sqel_type__pord\" as (\"p1\" bigint)"
    ],
    "statementsTable": [
      "create table \"dat\" (\"id\" bigint primary key not null, \"old\" text not null)"
    ],
    "table": [
      {
        "primMeta": {
          "colType": "bigint",
          "name": "id"
        },
        "tag": "SpinePrim"
      },
      {
        "primMeta": {
          "colType": "text",
          "name": "old"
        },
        "tag": "SpinePrim"
      }
    ],
    "types": [],
    "version": 0
  },
  {
    "name": {
      "tag": "table",
      "value": "dat"
    },
    "statementsMigration": [
      "alter table \"dat\" drop column \"size\"",
      "alter table \"dat\" add column \"number\" bigint",
      "update dat set \"number\" = 15",
      "alter table \"dat\" alter column \"number\" set not null",
      "alter type \"sqel_type__pord\" add attribute \"p2\" text"
    ],
    "statementsTable": [
      "create table \"dat\" (\"id\" bigint primary key not null, \"size\" bigint not null, \"pord\" \"sqel_type__pord\" not null)",
      "create type \"sqel_type__pord\" as (\"p1\" bigint)"
    ],
    "table": [
      {
        "primMeta": {
          "colType": "bigint",
          "name": "id"
        },
        "tag": "SpinePrim"
      },
      {
        "primMeta": {
          "colType": "bigint",
          "name": "size"
        },
        "tag": "SpinePrim"
      },
      {
        "compMeta": {
          "colType": "sqel_type__pord",
          "name": "pord"
        },
        "compSort": {
          "tag": "CompProd"
        },
        "sub": [],
        "tag": "SpineNest"
      },
      {
        "primMeta": {
          "colType": "bigint",
          "name": "number"
        },
        "tag": "SpinePrim"
      }
    ],
    "types": [
      {
        "meta": {
          "colType": "sqel_type__pord",
          "name": "pord"
        },
        "name": {
          "tag": "comp",
          "value": "sqel_type__pord"
        },
        "sub": [
          {
            "primMeta": {
              "colType": "bigint",
              "name": "p1"
            },
            "tag": "SpinePrim"
          }
        ]
      }
    ],
    "version": 1
  },
  {
    "name": {
      "tag": "table",
      "value": "dat"
    },
    "statementsMigration": [
      "alter table \"dat\" rename column \"number\" to \"num\"",
      "alter table \"dat\" add column \"name\" text",
      "update dat set \"name\" = \"vunqach\"",
      "alter table \"dat\" alter column \"name\" set not null"
    ],
    "statementsTable": [
      "create table \"dat\" (\"id\" bigint primary key not null, \"number\" bigint not null, \"pord\" \"sqel_type__pord\" not null)",
      "create type \"sqel_type__pord\" as (\"p1\" bigint, \"p2\" text)"
    ],
    "table": [
      {
        "primMeta": {
          "colType": "bigint",
          "name": "id"
        },
        "tag": "SpinePrim"
      },
      {
        "primMeta": {
          "colType": "bigint",
          "name": "number"
        },
        "tag": "SpinePrim"
      },
      {
        "compMeta": {
          "colType": "sqel_type__pord",
          "name": "pord"
        },
        "compSort": {
          "tag": "CompProd"
        },
        "sub": [],
        "tag": "SpineNest"
      }
    ],
    "types": [
      {
        "meta": {
          "colType": "sqel_type__pord",
          "name": "pord"
        },
        "name": {
          "tag": "comp",
          "value": "sqel_type__pord"
        },
        "sub": [
          {
            "primMeta": {
              "colType": "text",
              "name": "p1"
            },
            "tag": "SpinePrim"
          },
          {
            "primMeta": {
              "colType": "text",
              "name": "p2"
            },
            "tag": "SpinePrim"
          }
        ]
      },
      {
        "meta": {
          "colType": "sqel_type__point",
          "name": "pord"
        },
        "name": {
          "tag": "comp",
          "value": "sqel_type__point"
        },
        "sub": [
          {
            "primMeta": {
              "colType": "bigint",
              "name": "p1"
            },
            "tag": "SpinePrim"
          },
          {
            "primMeta": {
              "colType": "bigint",
              "name": "p2"
            },
            "tag": "SpinePrim"
          }
        ]
      }
    ],
    "version": 2
  },
  {
    "name": {
      "tag": "table",
      "value": "dat"
    },
    "statementsMigration": [],
    "statementsTable": [
      "create table \"dat\" (\"id\" bigint primary key not null, \"name\" text not null, \"num\" bigint not null, \"pord\" \"sqel_type__pord\" not null)",
      "create type \"sqel_type__pord\" as (\"p1\" bigint, \"p2\" text)"
    ],
    "table": [
      {
        "primMeta": {
          "colType": "bigint",
          "name": "id"
        },
        "tag": "SpinePrim"
      },
      {
        "primMeta": {
          "colType": "text",
          "name": "name"
        },
        "tag": "SpinePrim"
      },
      {
        "primMeta": {
          "colType": "bigint",
          "name": "num"
        },
        "tag": "SpinePrim"
      },
      {
        "compMeta": {
          "colType": "sqel_type__pord",
          "name": "pord"
        },
        "compSort": {
          "tag": "CompProd"
        },
        "sub": [],
        "tag": "SpineNest"
      }
    ],
    "types": [
      {
        "meta": {
          "colType": "sqel_type__pord",
          "name": "pord"
        },
        "name": {
          "tag": "comp",
          "value": "sqel_type__pord"
        },
        "sub": [
          {
            "primMeta": {
              "colType": "bigint",
              "name": "p1"
            },
            "tag": "SpinePrim"
          },
          {
            "primMeta": {
              "colType": "text",
              "name": "p2"
            },
            "tag": "SpinePrim"
          }
        ]
      }
    ],
    "version": 3
  }
]|]
