# Personal Project Website

## Who, What, Why?

### Who
Me! I'm a professional computer toucher who likes strong, advanced type systems and Haskell in particular.

### Why
This is a toy website where I can keep my programming skills sharp while playing with various patterns and styles. I don't expect to use it for anything, and I doubt I will expose it to the world at large for a couple of reasons, chiefly SSL certificates.

### What
Currently this project is server driven and roughly follows the idea of "Hypermedia as the engine of application state" ([HATEOAS](https://en.wikipedia.org/wiki/HATEOAS)). To this end I'm using [HTMX](https://htmx.org/) on the front end to add the little bits of reactivity and to extend forms, buttons, and the like to support HTTP `DELETE`, `PUT`, etc. HTML forms only supporting `GET` and `POST` is a bit restrictive and makes writing a CRUD API feel clunky.

The API itself is written using `servant` and `servant-auth-server` for several reasons.
- Strong, type safe internal links. If I change the routes I will get type errors if something is missed.
- Authentication combinators. This takes a lot of the heavy lifting for the crypto code out of my hands and into libraries.
- Composable APIs for testing. I can graft on a second API for peeking and poking at the database state in tests without having to worry about accidentally exposing it in the main server.

Things I want to add over time:
- ~~Basic text editing so that this can turn into a blog-ish thing. I think it would be an achievable goal without having to pull in heavy dependencies like React, TinyMCE, etc. I don't plan on doing much more than paragraph chunks, basic web links, and images between paragraphs. Everything will be formatted by static CSS and the server will be picky about how it wants to handle the incoming data.~~ CommonMark it is, for now at least.
- User management. Much like the blog section, I think that this would be a good fit for the HTMX frontend as it doesn't need to do anything too fancy, so long as I have some way to do user management without full-page reloads everytime I click something.
- File uploads. Today computers are very good at talking to everyone and everything, just not in the ways we want. Sending files to friends and family without using email or a third party of poor-at-best reputation is a long standing issue. As such, a way of uploading and sharing files to/from well-known users is an interesting project in streaming arbitrarily large blobs of data.

  As always, XKCD always has something for any situation.

  ![xkcd 949](https://imgs.xkcd.com/comics/file_transfer.png)

## How

### Running The Server

Do this `cabal run Website` from the project root, or `docker-compose up`

Or if you are feeling fancy you can build the binary, optionally strip it, and then run that. You also need to be careful to copy the `static`, `db`, and `certificates` (if TLS is enabled) directories to wherever you are running the server.

#### First Admin Setup

The website has an _unprotected_ route for setting up an initial user. It will accept any new user registration until an Admin user is created.
The following curl command is an example for setting up an Admin user for a local running server.
`curl -k -v -X POST -d "group=Admin&email=foo&password=bar" -H "Content-Type: application/x-www-form-urlencoded" "https://localhost:8080/register"`

#### Tests
Run this `cabal run Website-test`, or `docker-compose up test --build`. It will create a new testing database and stomp all over that as it throws requests left, right, and all over the place.

### TLS

Support for TLS is enabled via the `tls` cabal flag.
  - Cabal: `cabal build -f tls` or `cabal run -f tls`
  - Docker: `docker build --build-arg flags=tls .`
  - Docker Compose: `docker-compose build --build-arg flags=tls`

#### Certificates

Create, link, or copy a TLS certificate and key file into the project `certificates` directory with the name `certificate.pem` and `key.pem`. These files aren't automatically reloaded if they change while the server is runnning.