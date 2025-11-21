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
-   Handles standard Protobuf types (`string`, `int32`, etc.) mapping them to Malli predicates.
-   Supports registry generation for multiple messages.
-   Babashka compatible.

## Usage

### Prerequisites

-   Clojure or Babashka

### Parsing a file

```clojure
(require '[proto-to-malli.core :as ptm])

(def schema (ptm/parse-file "resources/simple.proto"))

;; Output example:
;; [:schema
;;  {:registry
;;   {:Person
;;    [:map
;;     [:name string?]
;;     [:id int?]
;;     [:email string?]]}}
;;  :Person]
```

## Development

### Directory Structure

-   `src/proto_to_malli`: Source code.
-   `test/proto_to_malli`: Tests.
-   `resources`: Example `.proto` files and expected `.edn` outputs.

### Dependencies

-   [Instaparse](https://github.com/ Engelberg/instaparse)
-   [Malli](https://github.com/metosin/malli) (optional, for usage of the schema)

## License

Open source.
