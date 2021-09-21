cd apache-jena-fuseki-3.16.0

start AISA-fuseki-server-mem.bat
cd ..

java -jar MapperB2.jar 1

taskkill /f /im java.exe


cd apache-jena-fuseki-3.16.0

start AISA-fuseki-server-mem.bat
cd ..

java -jar MapperB2.jar 10

taskkill /f /im java.exe


cd apache-jena-fuseki-3.16.0

start AISA-fuseki-server-mem.bat
cd ..

java -jar MapperB2.jar 100

taskkill /f /im java.exe


cd apache-jena-fuseki-3.16.0

start AISA-fuseki-server-mem.bat
cd ..

java -jar MapperB2.jar 200

taskkill /f /im java.exe


cd apache-jena-fuseki-3.16.0

start AISA-fuseki-server-mem.bat
cd ..

java -jar MapperB2.jar 300

taskkill /f /im java.exe


cd apache-jena-fuseki-3.16.0

start AISA-fuseki-server-mem.bat
cd ..

java -jar MapperB2.jar 1000

taskkill /f /im java.exe

timeout /t 5 /nobreak >nul



