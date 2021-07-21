@REM starting Fuseki with transactional in-memory dataset configuration
@echo off
java -Xmx1200M -jar fuseki-server.jar --config=AISA-config-memtx.ttl


