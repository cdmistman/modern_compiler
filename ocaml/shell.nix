let
	nixpkgs = <nixpkgs>;
	ocaml-overlay = builtins.fetchTarball "https://github.com/nix-ocaml/nix-overlays/archive/master.tar.gz";
in with import nixpkgs {
		overlays = [
			(import "${ocaml-overlay}/overlay" nixpkgs)
		];
	};
let
	ocamlPackages = pkgs.recurseIntoAttrs pkgs.ocaml-ng.ocamlPackages_5_0;
in
pkgs.mkShell {
	nativeBuildInputs = (with pkgs; [
		dune_3
		gmp
		# ocamlformat
		pkg-config
	]) ++ (with ocamlPackages; [
		findlib
		menhir
		ocaml
		ocaml-lsp

		# TODO: enable when cohttp-eio becomes available on nix
		# domain-name
		# eio_main
		# eio
		# mirage-crypto-rng
		# mirage-crypto-rng-eio
		# ounit
		# ppx_expect
		# tls-eio
		# zarith
	]) ++ (lib.optionals stdenv.isDarwin (with pkgs.darwin.apple_sdk.frameworks; [
		CoreServices
		Foundation
	]));

	buildInputs = with pkgs; [
	];
}
