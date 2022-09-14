dhall format defaults.dhall
for d in hipsql-api hipsql-client hipsql-server hipsql-monad
do
    dhall format ${d}/package.dhall
    dhall-hpack-cabal --package-dhall=${d}/package.dhall
done