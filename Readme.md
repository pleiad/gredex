Structure of this project:
```
.
├── build.sbt
├── frontend
│   ├── package.json
│   ├── src
│   │   ├── components
│   │   │   ├── DerivationTree.tsx
│   │   │   ├── Editor.tsx
│   │   │   ├── Home.tsx
│   │   │   └── Syntax.tsx
│   │   ├── App.tsx
│   │   └── main.tsx
├── src
│   ├── main
│   │   ├── scala
│   │   │   ├── api
│   │   │   ├── lang
│   │   │   │   ├── syntax
│   │   │   │   ├── typing
│   │   │   │   ├── runtime
│   │   │   │   └── Parser.scala
│   │   └── resources
│   └── test
│       └── scala
```

To compile frontend
```bash
sbt buildFrontend
```

To compile backend
```bash
sbt compile
```

To run
```bash
sbt run api.Api
```