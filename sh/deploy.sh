echo "build app ..."
npm run build
echo "uploading to cloud ..."
scp -r ./dist/* root@138.197.81.6:/var/www/html/
