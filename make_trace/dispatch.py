import sys

from filterPass import filterPass
from anfPass import anfPass

passName = sys.argv[1]
if passName == "filter":
    filterPass(sys.argv[2], sys.argv[3])
elif passName == "anf":
    anfPass(sys.argv[2])

# step "2" of pipeline:
# subprocess.check_output(["docker", "run", "-v", "/Users/benjamin/LessTemporaryDownloads/may-to-july-2017-server-logs/data:/app/data", "python-munge"])
