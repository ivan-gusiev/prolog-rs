# Build Stage
FROM rust AS build-stage

ADD . /usr/src/prolog-rs
WORKDIR /usr/src/prolog-rs

RUN cargo build --release

# Final Stage
FROM scratch

ARG GIT_COMMIT
ARG VERSION
LABEL REPO="https://github.com/ivan-gusiev/prolog-rs"
LABEL GIT_COMMIT=$GIT_COMMIT
LABEL VERSION=$VERSION

WORKDIR /usr/local/bin

COPY --from=build-stage /usr/src/prolog-rs/bin/prolog-rs /opt/prolog-rs/bin/
RUN chmod +x /usr/local/bin/prolog-rs

CMD /usr/local/bin/prolog-rs
