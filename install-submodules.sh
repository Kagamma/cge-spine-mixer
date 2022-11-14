rm -rf src/components
rm -rf src/components/spine-runtimes

cd src
cd components
git clone https://github.com/Kagamma/spine-runtimes.git
cd spine-runtimes
git checkout 4.1
cd ..
cd ..
cd ..
