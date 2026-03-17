# Iapetus — Technical Overview

Iapetus is an early-stage web application for amateur astronomers (observing session prep, logs, galleries, community). The codebase is a **Scala 3 SBT multi-module project** that is currently scaffold/placeholder code — no real business logic is implemented yet.

---

## Core Components

The project is structured as three SBT sub-projects plus an aggregate root, all using **Scala 3.3.4**:

**`common/`** (`common/src/main/scala/Shared.scala`)
- Shared JVM/JS code compiled for both targets.
- Currently, contains a single `Shared` object with one stub method. Intended to hold data models and logic shared between front and back ends.

**`backend/`** (`backend/src/main/scala/Main.scala`)
- JVM-only module; depends on `common`.
- Entry point is a standard `object Main` with a `main` method — currently just a `println` stub.
- Key declared dependencies: `http4s-ember-server` and `http4s-dsl` (v0.23.26), which are a functional, cats-effect-based HTTP server and routing DSL. No http4s routes or server setup are written yet.

**`frontend/`** (`frontend/src/main/scala/Main.scala`)
- Scala.js target; depends on `common`.
- Compiled to JavaScript via the `sbt-scalajs` plugin (v1.15.0). Uses `scalaJSUseMainModuleInitializer := true`, so `Main.main` is the JS entry point.
- Currently directly calls `scalajs-dom` to append a `<p>` element — no Laminar reactive UI is wired up yet despite `laminar` 15.0.0 being declared as a dependency.
- A `frontend/testData/index.html` loads `frontend-fastopt.js` for manual browser testing of the compiled output.

**`root`**
- Aggregate project at the repo root; aggregates `backend`, `frontend`, and `common` so `sbt compile`, `sbt test`, etc. run across all three.

---

## Component Interactions

Currently, there is **no runtime communication** between frontend and backend — both are stubs. The intended architecture based on declared dependencies is:

- The `common` module acts as a **shared contract layer** — data types defined there compile to both JVM (backend) and JS (frontend), eliminating duplication of API models.
- The frontend (Scala.js + Laminar) will run in the browser and make HTTP calls to the backend.
- The backend (http4s Ember) will expose a REST or RPC API consumed by the frontend. http4s uses a **tagless final / cats-effect `IO`** execution model — all handlers are pure `IO` computations assembled via the http4s DSL.
- Laminar is a **reactive/FRP UI library** — the intended UI model is declarative reactive signals rather than imperative DOM manipulation (the current DOM call in `Main.scala` is just initial scaffolding).

---

## Deployment Architecture

No deployment configuration (Dockerfile, CI, deployment scripts) exists yet. The stated plan from `IapetusNotes/Technology Stack.md`:

- **Development**: Local Windows machine (current), targeting a **headless Linux dev server** ("Dustdevil") for integration testing on LAN.
- **Production**: A **VPS** (e.g., DigitalOcean, Hetzner) with potential Docker containerization.
- **Build pipeline**:
  1. `sbt frontend/fastOptJS` (or `fullOptJS` for production) compiles Scala → JavaScript.
  2. The resulting JS bundle is served as a static asset alongside the backend server.
  3. `sbt backend/run` starts the http4s Ember server (JVM process).
- **Build tooling**: SBT as the primary build tool; **Bloop** (`.bloop/`) is configured as a build server for IDE integration (IntelliJ IDEA via `.idea/`, Metals via `.metals/`); **VSCode** settings are also present.

---

## Runtime Behavior

**Backend** (intended, not yet implemented):
- On startup, the http4s Ember server would bind to a port and serve requests via cats-effect `IO`. The Ember backend is built on **fs2** streams and uses **non-blocking I/O** throughout.
- Routing would be defined via http4s DSL pattern matching on `Method / path` combinators.
- Error handling in http4s is compositional — `HttpRoutes` returns `OptionT[IO, Response]`; unmatched routes fall through to a 404 handler.

**Frontend** (intended):
- On page load the JS entry point runs `Main.main`, which will eventually initialize a Laminar reactive application and mount it into the DOM.
- Laminar state is managed via `Var`/`Signal`/`EventStream` — UI updates are driven by reactive dataflow without direct DOM mutation.

**Current actual runtime behavior** is trivial: the backend prints a string and exits; the frontend appends a static `<p>` tag to the document body.

---

## Alternate Implementations (non-active)

`alternates/React/` — A Node.js/Express backend (CRUD REST API over an in-memory array, port 3000) with a MongoDB driver dependency (`mongoDemo.mjs`) and a React frontend skeleton. No active development.

`alternates/Ada/` — An Ada program with an `astronomical_time` package and an HTTP listener; incomplete, shuts itself off after one minute. No active development.
