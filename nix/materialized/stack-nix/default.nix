{
  extras = hackage:
    {
      packages = {
        "OddWord" = (((hackage.OddWord)."1.0.2.0").revisions).default;
        "command" = (((hackage.command)."0.1.1").revisions).default;
        "hspec" = (((hackage.hspec)."2.8.2").revisions).default;
        "hspec-core" = (((hackage.hspec-core)."2.8.2").revisions).default;
        "hspec-discover" = (((hackage.hspec-discover)."2.8.2").revisions).default;
        "blockfrost-api" = (((hackage.blockfrost-api)."0.4.0.0").revisions).default;
        "blockfrost-client" = (((hackage.blockfrost-client)."0.4.0.0").revisions).default;
        "blockfrost-client-core" = (((hackage.blockfrost-client-core)."0.4.0.0").revisions).default;
        "quickcheck-quid" = (((hackage.quickcheck-quid)."0.0.1").revisions).default;
        "servant-multipart-client" = (((hackage.servant-multipart-client)."0.12.1").revisions).default;
        "cryptonite" = (((hackage.cryptonite)."0.27").revisions).default;
        "Cabal" = (((hackage.Cabal)."3.4.0.0").revisions).default;
        "containers" = (((hackage.containers)."0.6.4.1").revisions).default;
        "dns" = (((hackage.dns)."3.0.4").revisions).default;
        "network" = (((hackage.network)."3.1.2.1").revisions).default;
        "ral" = (((hackage.ral)."0.1").revisions).default;
        "recursion-schemes" = (((hackage.recursion-schemes)."5.1.3").revisions).default;
        "text" = (((hackage.text)."1.2.4.0").revisions).default;
        "Win32" = (((hackage.Win32)."2.6.2.0").revisions).default;
        "async-timer" = (((hackage.async-timer)."0.2.0.0").revisions).default;
        "beam-core" = (((hackage.beam-core)."0.9.1.0").revisions).default;
        "beam-migrate" = (((hackage.beam-migrate)."0.5.1.0").revisions).default;
        "beam-sqlite" = (((hackage.beam-sqlite)."0.5.1.0").revisions).default;
        "canonical-json" = (((hackage.canonical-json)."0.6.0.0").revisions).default;
        "composition-prelude" = (((hackage.composition-prelude)."3.0.0.2").revisions).default;
        "constraints-extras" = (((hackage.constraints-extras)."0.3.1.0").revisions).default;
        "ip" = (((hackage.ip)."1.5.1").revisions).default;
        "gray-code" = (((hackage.gray-code)."0.3.1").revisions).default;
        "lazy-search" = (((hackage.lazy-search)."0.1.2.1").revisions).default;
        "lazysmallcheck" = (((hackage.lazysmallcheck)."0.6").revisions).default;
        "libsystemd-journal" = (((hackage.libsystemd-journal)."1.4.4").revisions).default;
        "markov-chain-usage-model" = (((hackage.markov-chain-usage-model)."0.0.0").revisions).default;
        "micro-recursion-schemes" = (((hackage.micro-recursion-schemes)."5.0.2.2").revisions).default;
        "monoidal-containers" = (((hackage.monoidal-containers)."0.6.0.1").revisions).default;
        "moo" = (((hackage.moo)."1.2").revisions).default;
        "nothunks" = (((hackage.nothunks)."0.1.2").revisions).default;
        "partial-order" = (((hackage.partial-order)."0.2.0.0").revisions).default;
        "quickcheck-state-machine" = (((hackage.quickcheck-state-machine)."0.7.0").revisions).default;
        "regex-posix-clib" = (((hackage.regex-posix-clib)."2.7").revisions).default;
        "row-types" = (((hackage.row-types)."1.0.1.0").revisions).default;
        "servant-subscriber" = (((hackage.servant-subscriber)."0.7.0.0").revisions).default;
        "dom-lt" = (((hackage.dom-lt)."0.2.2.1").revisions).default;
        "servant-websockets" = (((hackage.servant-websockets)."2.0.0").revisions).default;
        "size-based" = (((hackage.size-based)."0.1.2.0").revisions).default;
        "statistics-linreg" = (((hackage.statistics-linreg)."0.3").revisions).default;
        "streaming-binary" = (((hackage.streaming-binary)."0.2.2.0").revisions).default;
        "time-interval" = (((hackage.time-interval)."0.1.1").revisions).default;
        "time-out" = (((hackage.time-out)."0.2").revisions).default;
        "hspec-golden" = (((hackage.hspec-golden)."0.2.0.0").revisions).default;
        "transformers-except" = (((hackage.transformers-except)."0.1.1").revisions).default;
        "Unique" = (((hackage.Unique)."0.4.7.6").revisions).default;
        "binary" = (((hackage.binary)."0.8.8.0").revisions).default;
        "parsec" = (((hackage.parsec)."3.1.14.0").revisions).default;
        dbvar = ./dbvar.nix;
        cardano-wallet-core = ./cardano-wallet-core.nix;
        cardano-wallet-core-integration = ./cardano-wallet-core-integration.nix;
        cardano-wallet-cli = ./cardano-wallet-cli.nix;
        cardano-wallet-launcher = ./cardano-wallet-launcher.nix;
        cardano-numeric = ./cardano-numeric.nix;
        text-class = ./text-class.nix;
        cardano-wallet-test-utils = ./cardano-wallet-test-utils.nix;
        cardano-wallet = ./cardano-wallet.nix;
        strict-non-empty-containers = ./strict-non-empty-containers.nix;
        cardano-addresses-cli = ./.stack-to-nix.cache.0;
        cardano-addresses = ./.stack-to-nix.cache.1;
        servant-openapi3 = ./.stack-to-nix.cache.2;
        openapi3 = ./.stack-to-nix.cache.3;
        optparse-applicative-fork = ./.stack-to-nix.cache.4;
        bech32 = ./.stack-to-nix.cache.5;
        bech32-th = ./.stack-to-nix.cache.6;
        base-deriving-via = ./.stack-to-nix.cache.7;
        cardano-binary = ./.stack-to-nix.cache.8;
        cardano-binary-test = ./.stack-to-nix.cache.9;
        cardano-crypto-class = ./.stack-to-nix.cache.10;
        cardano-crypto-praos = ./.stack-to-nix.cache.11;
        cardano-crypto-tests = ./.stack-to-nix.cache.12;
        orphans-deriving-via = ./.stack-to-nix.cache.13;
        cardano-slotting = ./.stack-to-nix.cache.14;
        strict-containers = ./.stack-to-nix.cache.15;
        measures = ./.stack-to-nix.cache.16;
        cardano-crypto = ./.stack-to-nix.cache.17;
        cardano-ledger-alonzo = ./.stack-to-nix.cache.18;
        cardano-ledger-alonzo-test = ./.stack-to-nix.cache.19;
        cardano-ledger-babbage = ./.stack-to-nix.cache.20;
        cardano-ledger-babbage-test = ./.stack-to-nix.cache.21;
        byron-spec-chain = ./.stack-to-nix.cache.22;
        cardano-crypto-wrapper = ./.stack-to-nix.cache.23;
        cardano-crypto-test = ./.stack-to-nix.cache.24;
        byron-spec-ledger = ./.stack-to-nix.cache.25;
        cardano-ledger-byron = ./.stack-to-nix.cache.26;
        cardano-ledger-byron-test = ./.stack-to-nix.cache.27;
        cardano-ledger-shelley = ./.stack-to-nix.cache.28;
        cardano-ledger-shelley-test = ./.stack-to-nix.cache.29;
        cardano-ledger-shelley-ma = ./.stack-to-nix.cache.30;
        cardano-ledger-shelley-ma-test = ./.stack-to-nix.cache.31;
        cardano-ledger-core = ./.stack-to-nix.cache.32;
        cardano-ledger-pretty = ./.stack-to-nix.cache.33;
        cardano-protocol-tpraos = ./.stack-to-nix.cache.34;
        cardano-data = ./.stack-to-nix.cache.35;
        vector-map = ./.stack-to-nix.cache.36;
        set-algebra = ./.stack-to-nix.cache.37;
        small-steps = ./.stack-to-nix.cache.38;
        small-steps-test = ./.stack-to-nix.cache.39;
        non-integral = ./.stack-to-nix.cache.40;
        cardano-api = ./.stack-to-nix.cache.41;
        cardano-cli = ./.stack-to-nix.cache.42;
        cardano-git-rev = ./.stack-to-nix.cache.43;
        cardano-node = ./.stack-to-nix.cache.44;
        cardano-node-chairman = ./.stack-to-nix.cache.45;
        trace-dispatcher = ./.stack-to-nix.cache.46;
        trace-resources = ./.stack-to-nix.cache.47;
        trace-forward = ./.stack-to-nix.cache.48;
        cardano-prelude = ./.stack-to-nix.cache.49;
        cardano-prelude-test = ./.stack-to-nix.cache.50;
        cardano-sl-x509 = ./.stack-to-nix.cache.51;
        flat = ./.stack-to-nix.cache.52;
        goblins = ./.stack-to-nix.cache.53;
        hedgehog-extras = ./.stack-to-nix.cache.54;
        contra-tracer = ./.stack-to-nix.cache.55;
        iohk-monitoring = ./.stack-to-nix.cache.56;
        lobemo-backend-aggregation = ./.stack-to-nix.cache.57;
        lobemo-backend-ekg = ./.stack-to-nix.cache.58;
        lobemo-backend-monitoring = ./.stack-to-nix.cache.59;
        lobemo-backend-trace-forwarder = ./.stack-to-nix.cache.60;
        lobemo-scribe-systemd = ./.stack-to-nix.cache.61;
        tracer-transformers = ./.stack-to-nix.cache.62;
        purescript-bridge = ./.stack-to-nix.cache.63;
        monoidal-synchronisation = ./.stack-to-nix.cache.64;
        network-mux = ./.stack-to-nix.cache.65;
        ouroboros-consensus = ./.stack-to-nix.cache.66;
        ouroboros-consensus-test = ./.stack-to-nix.cache.67;
        ouroboros-consensus-byron = ./.stack-to-nix.cache.68;
        ouroboros-consensus-byronspec = ./.stack-to-nix.cache.69;
        ouroboros-consensus-byron-test = ./.stack-to-nix.cache.70;
        ouroboros-consensus-protocol = ./.stack-to-nix.cache.71;
        ouroboros-consensus-shelley = ./.stack-to-nix.cache.72;
        ouroboros-consensus-shelley-test = ./.stack-to-nix.cache.73;
        ouroboros-consensus-cardano = ./.stack-to-nix.cache.74;
        ouroboros-consensus-cardano-test = ./.stack-to-nix.cache.75;
        ouroboros-network = ./.stack-to-nix.cache.76;
        ouroboros-network-framework = ./.stack-to-nix.cache.77;
        ouroboros-network-testing = ./.stack-to-nix.cache.78;
        cardano-client = ./.stack-to-nix.cache.79;
        ntp-client = ./.stack-to-nix.cache.80;
        ouroboros-consensus-mock = ./.stack-to-nix.cache.81;
        io-classes = ./.stack-to-nix.cache.82;
        io-sim = ./.stack-to-nix.cache.83;
        strict-stm = ./.stack-to-nix.cache.84;
        typed-protocols = ./.stack-to-nix.cache.85;
        typed-protocols-cborg = ./.stack-to-nix.cache.86;
        typed-protocols-examples = ./.stack-to-nix.cache.87;
        plutus-core = ./.stack-to-nix.cache.88;
        plutus-ledger-api = ./.stack-to-nix.cache.89;
        plutus-tx = ./.stack-to-nix.cache.90;
        plutus-tx-plugin = ./.stack-to-nix.cache.91;
        plutus-ghc-stub = ./.stack-to-nix.cache.92;
        prettyprinter-configurable = ./.stack-to-nix.cache.93;
        word-array = ./.stack-to-nix.cache.94;
        ekg-forward = ./.stack-to-nix.cache.95;
        cardano-config = ./.stack-to-nix.cache.96;
        servant-purescript = ./.stack-to-nix.cache.97;
        Win32-network = ./.stack-to-nix.cache.98;
        ekg-json = ./.stack-to-nix.cache.99;
        hw-aeson = ./.stack-to-nix.cache.100;
        };
      compiler.version = "8.10.7";
      compiler.nix-name = "ghc8107";
      };
  resolver = "lts-18.21";
  modules = [
    ({ lib, ... }:
      {
        packages = {
          "cryptonite" = {
            flags = { "support_rdrand" = lib.mkOverride 900 false; };
            };
          "cardano-crypto-praos" = {
            flags = { "external-libsodium-vrf" = lib.mkOverride 900 false; };
            };
          "zip" = { flags = { "disable-bzip2" = lib.mkOverride 900 true; }; };
          };
        })
    { packages = { "$locals" = { ghcOptions = [ "-fwrite-ide-info" ]; }; }; }
    ({ lib, ... }:
      { planned = lib.mkOverride 900 true; })
    ];
  compiler = "ghc-8.10.7";
  }