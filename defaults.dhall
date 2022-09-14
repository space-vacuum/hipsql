{ pkg =
  { version = "0.1.0.0"
  , github = "simspace/hipsql"
  , license = "BSD3"
  , author = "Cary Robbins"
  , maintainer = "carymrobbins@gmail.com"
  , copyright = "2021 SimSpace"
  , description =
      "Please see the README on GitHub at <https://github.com/simspace/hipsql#readme>"
  , extra-source-files = [ "CHANGELOG.md" ]
  , ghc-options = [ "-Wall" ]
  , library.source-dirs = [ "library" ]
  , dependencies = [ "base >= 4.7 && < 5" ]
  }
, exe-ghc-options = [ "-threaded", "-rtsopts", "-with-rtsopts=-N" ]
}
