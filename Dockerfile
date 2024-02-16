FROM ocaml/opam:alpine-3.18-ocaml-4.14 as build


WORKDIR /home/opam


COPY --chown=opam:opam . .

RUN make deps
RUN make
RUN make install

CMD rat
