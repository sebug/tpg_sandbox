TPG Sandbox
===========
In which I do some Haskell toying around with the TPG Open Data:
http://www.tpg.ch/web/open-data/donnees

You will have to obtain a key (link "Demander une clé") and store it under "~/.tpg_tests" in the following json format:
{ "api_key" : "c43cf880-7713-c818-8e64-eba0ea38914e" }

replacing of course with your own Guid.

As the executable names are still a bit messy, I suggest you just use

cabal build

and install only when it is a bit clearer how we should call them.

