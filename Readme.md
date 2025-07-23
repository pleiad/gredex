<div align="left" style="position: relative;">
<h1>GREDEX</h1>
<p align="left">
	<em>An interactive tool for exploring the semantics of gradually-typed programming languages.</em>
</p>
<p align="left">
	<img src="https://img.shields.io/github/license/pleiad/gredex?style=default&logo=opensourceinitiative&logoColor=white&color=0080ff" alt="license">
	<img src="https://img.shields.io/github/last-commit/pleiad/gredex?style=default&logo=git&logoColor=white&color=0080ff" alt="last-commit">
	<img src="https://img.shields.io/github/languages/top/pleiad/gredex?style=default&color=0080ff" alt="repo-top-language">
	<img src="https://img.shields.io/github/languages/count/pleiad/gredex?style=default&color=0080ff" alt="repo-language-count">
</p>
</div>
<br clear="right">

## Table of Contents

- [Overview](#overview)
- [Features](#features)
- [Project Structure](#project-structure)
- [Getting Started](#getting-started)
  - [Prerequisites](#prerequisites)
  - [Installation and Compilation](#installation-and-compilation)
  - [Running the Frontend in Development Mode](#running-the-frontend-in-development-mode)
- [Running the Application](#running-the-application)
- [Using Docker](#using-docker)
  - [Build the Docker Image](#build-the-docker-image)
  - [Run the Docker Container](#run-the-docker-container)
- [Testing](#testing)
- [Extending Gredex](#extending-gredex)
- [License](#license)
- [Acknowledgments](#acknowledgments)

---

## Overview

Gredex is a web-based application designed to help researchers and educators explore the semantics of gradually-typed programming languages. It provides an interactive interface for visualizing type derivations, step-by-step program reductions, and runtime evidence evolution. Gredex is particularly useful for prototyping gradual languages, debugging type systems, and teaching gradual typing concepts.

---

## Features

- **Extensible Gradual Language**: A core gradual language supporting numbers, booleans, functions, pairs, sums, and fixpoints. Users can extend the language to experiment with new features and typing disciplines.
- **Type Derivation Visualization**: Displays complete typing derivations as interactive trees, showing the rules applied to each term and subterm.
- **Step-wise Reduction**: Visualizes program execution step by step, highlighting the active reducible expression (redex) and showing intermediate terms and their typing derivations.
- **Runtime Evidence Display**: Tracks and displays runtime typing information, including evidence and consistency judgments, to help debug type errors.
- **Example Gallery**: Includes predefined examples illustrating both successful and failing scenarios. Examples are editable and can be re-executed.
- **Self-contained Deployment**: Does not require external databases or services, making it lightweight and easy to deploy.

---

## Project Structure

```
.
├── build.sbt                # Build configuration for the Scala backend
├── frontend                 # Frontend implementation in React/TypeScript
│   ├── package.json         # Frontend dependencies
│   ├── src                  # Source code for the frontend
│   │   ├── components       # React components
│   │   │   ├── DerivationTree.tsx  # Visualization of derivation trees
│   │   │   ├── Editor.tsx          # Code editor with syntax highlighting
│   │   │   ├── Home.tsx            # Main interface and example gallery
│   │   │   └── Syntax.tsx          # Syntax-related utilities
│   │   ├── App.tsx           # Application shell and routing
│   │   └── main.tsx          # Entry point for the frontend
├── src                      # Backend implementation in Scala
│   ├── main
│   │   ├── scala            # Scala source code
│   │   │   ├── api          # REST API endpoints
│   │   │   ├── lang         # Gradual language library
│   │   │   │   ├── syntax   # Syntax definitions
│   │   │   │   ├── typing   # Typing and elaboration logic
│   │   │   │   ├── runtime  # Runtime evaluation
│   │   │   │   └── Parser.scala  # Parser for the gradual language
│   │   └── resources        # Static resources and configuration
│   └── test                 # Test cases for the backend
│       └── scala
```

---

## Getting Started

### Prerequisites

Before getting started with Gredex, ensure your runtime environment meets the following requirements:

- **Frontend**: Node.js v20, npm
- **Backend**: Scala 3.3.5, sbt 1.10.11
- **Dependencies**:
  - Pekko (v1.0.2), Pekko HTTP (v1.0.1) (backend)
  - Material-UI (7.1.1), MathJax (3.2.2), React (19.1.0), and TypeScript (5.8.3) (frontend)

### Installation and Compilation

1. Clone the Gredex repository:

   ```sh
   git clone https://github.com/pleiad/gredex
   cd gredex
   ```

2. Compile the frontend:

   ```sh
   sbt buildFrontend
   ```

3. Compile the backend:

   ```sh
   sbt compile
   ```

### Running the Frontend in Development Mode

To run the frontend in development mode, navigate to the `frontend` directory and use the following commands:

1. Navigate to the `frontend` directory:

   ```sh
   cd frontend
   ```

2. Install the required dependencies (if not already installed):

   ```sh
   npm install
   ```

3. Start the development server:
   ```sh
   npm run dev
   ```

The frontend will be accessible at `http://localhost:5173` (or another port if specified).

---

## Running the Application

3. Start the application:

   ```sh
   sbt run
   ```

The application will launch the backend server and serve the frontend assets. Open your browser and navigate to `http://localhost:8080` to access Gredex.

---

## Using Docker

You can build and run Gredex using Docker to ensure a consistent and reproducible environment.

### Build the Docker Image

From the root directory of the project, run the following command to build the Docker image:

```sh
docker build -t gredex:latest .
```

### Run the Docker Container

To start the application using the Docker container, run:

```sh
docker run -p 8080:8080 gredex:latest
```

The application will be accessible at `http://localhost:8080`.

---

## Testing

Run the test suite using the following command:

```sh
sbt test
```

---

## Extending Gredex

Gredex's backend is powered by the `glang` library, a modular Scala library designed for gradual language semantics. Extending the language involves modifying or adding to the following core modules:

1. **Syntax Module (`syntax/`)**:

   - Define new abstract syntax tree (AST) nodes for your language constructs.
   - Update or create new classes in `syntax` to represent the constructs.
   - Ensure that each new construct implements the required methods, such as `toLatex` for rendering.

2. **Parser (`Parser.scala`)**:

   - Update the parser to recognize the new constructs in the source code.
   - Ensure that the parser produces the correct AST nodes for the constructs.

3. **Typing/Typed Elaboration Module (`typing/`)**:

   - Add typing rules for the new constructs.
   - Extend the typechecking logic to handle the new constructs.
   - If introducing a new type discipline, define the rules for consistency and evidence generation.

4. **Runtime Module (`runtime/`)**:

   - Implement the reduction semantics for the new constructs.
   - Define how the constructs behave during program execution.
   - Update the runtime evaluation logic to handle the new constructs.

5. **Frontend (`frontend/src/components/`)**:
   - Update the React components to incorporate the new syntax in the syntax description popup and ensure it is properly displayed in the user interface.

By following these steps, you can extend Gredex to support new language features or typing disciplines, enabling experimentation with novel gradual typing semantics.

---

## License

This project is licensed under the MIT License. See the `LICENSE` file for details.

---

## Acknowledgments

Gredex has been developed by the PLEIAD research group at the University of Chile. It has been used in multiple research projects and educational settings to explore and teach gradual typing semantics. Special thanks to all contributors and users who have provided feedback and support.
