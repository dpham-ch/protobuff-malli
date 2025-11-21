# Proto to Malli

A Clojure library (compatible with Babashka) that parses Google Protobuf (`.proto`) files and converts them into Metosin Malli schemas.

## Goal

The goal of this project is to provide a lightweight parser for Google Protobuf files using Instaparse, outputting the schema in Metosin Malli hiccup format with a registry.

The output format follows:
```clojure
[:schema {:registry ...} MessageType]
```

## Features

-   Parses `proto3` syntax.
-   Converts Protobuf messages to Malli `[:map ...]` schemas.
-   Supports all scalar types, enums, nested messages, repeated fields, maps, and oneof.
-   **Target Support**: Generate schemas optimized for Clojure, Java, or JavaScript type systems.
-   Babashka compatible.

## Usage

### Prerequisites

-   Clojure or Babashka

### Parsing a file

```clojure
(require '[proto-to-malli.core :as ptm])

;; Default (Clojure/ClojureScript friendly types)
(def schema (ptm/parse-file "resources/simple.proto"))

;; Java specific types (e.g., :long for int64, :float for float)
(def java-schema (ptm/parse-file "resources/simple.proto" {:target :java}))

;; JavaScript specific types (e.g., :string for int64/bytes, :double for floats)
(def js-schema (ptm/parse-file "resources/simple.proto" {:target :js}))
```

### Supported Type Mappings

| Proto Type | Clojure (Default) | Java (:java) | JavaScript (:js) |
|------------|-------------------|--------------|------------------|
| double     | :double           | :double      | :double          |
| float      | :double           | :float       | :double          |
| int32      | :int              | :int         | :int             |
| int64      | :int              | :long        | :string          |
| uint32     | :int              | :int         | :int             |
| uint64     | :int              | :long        | :string          |
| sint32     | :int              | :int         | :int             |
| sint64     | :int              | :long        | :string          |
| fixed32    | :int              | :int         | :int             |
| fixed64    | :int              | :long        | :string          |
| sfixed32   | :int              | :int         | :int             |
| sfixed64   | :int              | :long        | :string          |
| bool       | :boolean          | :boolean     | :boolean         |
| string     | :string           | :string      | :string          |
| bytes      | :bytes            | :bytes       | :string          |

## Development

### Directory Structure

-   `src/proto_to_malli`: Source code.
-   `test/proto_to_malli`: Tests.
-   `resources`: Example `.proto` files and expected `.edn` outputs.

### Dependencies

-   [Instaparse](https://github.com/Engelberg/instaparse)
-   [Malli](https://github.com/metosin/malli) (optional, for usage of the schema)

## License

Open source.
