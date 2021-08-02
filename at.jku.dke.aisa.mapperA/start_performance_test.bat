cd apache-jena-fuseki-3.16.0

start AISA-fuseki-server-mem.bat
cd ..

java -jar MapperA.jar 1


timeout /t 5 /nobreak >nul

taskkill /f /im java.exe

