TPG Sandbox
===========
In which I do some Haskell toying around with the TPG Open Data:
http://www.tpg.ch/web/open-data/donnees

You will have to obtain a key (link "Demander une clé") and store it under "~/.tpg_tests" in the following json format:
{ "api_key" : "c43cf880-7713-c818-8e64-eba0ea38914e" }

replacing of course with your own Guid.

The code relies on the following packages (I haven't learned about proper packaging yet, so just cabal install ahead):

- HTTP
- aeson
