https://www.gnu.org/software/gawk/manual/html_node/gawkextlib.html



cd ~/src/awk

sudo apt install cmake
git clone https://github.com/Tencent/rapidjson.git 
cd rapidjson
cmake .
make
sudo make install
cd ..

sudo apt install libexpat-dev

sudo apt install texinfo
git clone ..../gawk.git
cd gawk
./configure --prefix=$HOME/gawk
make && make check && make install
