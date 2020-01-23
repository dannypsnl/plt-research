# Agda

### Installation

```
brew install agda
mkdir -p ~/.agda
git clone https://github.com/agda/agda-stdlib.git
echo "standard-library" > defaults
# !!! $HOME should be replaced with absolute path
echo "$HOME/.agda/agda-stdlib/standard-library.agda-lib" > libraries
```
