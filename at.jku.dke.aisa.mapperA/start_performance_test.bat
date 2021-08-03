cd apache-jena-fuseki-3.16.0

start AISA-fuseki-server-mem.bat
cd ..

C:\"Program Files"\Java\jdk-9.0.4\bin\java -jar MapperA.jar 1


timeout /t 5 /nobreak >nul

taskkill /f /im java.exe

