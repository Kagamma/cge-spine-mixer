rm -rf src/components

cd src
mkdir components
cd components
git clone https://github.com/Kagamma/spine-runtimes.git
cd spine-runtimes
git checkout 4.1
cd ..
cd ..
cd ..
