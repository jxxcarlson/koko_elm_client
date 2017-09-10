
echo "use src/deployment configuation"
cp ./robot/src/deploy/Configuration.elm ./src/
cp ./robot/src/deploy/webpack.config.js ./src/

echo "build app ..."
npm run build
echo "upload to cloud ..."
scp -r ./dist/* root@138.197.81.6:/var/www/html/
