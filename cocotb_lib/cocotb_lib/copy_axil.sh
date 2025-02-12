for NM in eth_mac; do
    mkdir -p "$NM"
    cp "$HOME/per/cocotbext-eth/cocotbext/eth/$NM.py" "./$NM/"
    echo "from .$NM import *" > "./$NM/__init__.py"
done
