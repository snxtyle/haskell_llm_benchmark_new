pip install -e .[dev]
./benchmark/benchmark.py glm-latest-v1 --model glm-latest --edit-format whole --threads 10 --num-tests 1 --exercises-dir polyglot-benchmark --new
clear
tmux
pip3 install tmux
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
brew install tmux
brew
echo >> /root/.bashrc
echo 'eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"' >> /root/.bashrc
eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
echo >> /root/.bashrc
echo 'eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"' >> /root/.bashrc
eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
brew install tmux
./benchmark/benchmark.py glm-latest-v1 --model glm-latest --edit-format whole --threads 10 --num-tests 1 --exercises-dir polyglot-benchmark --new
./benchmark/benchmark.py glm-latest-v1 --model glm-latest --edit-format whole --threads 10 --num-tests 1 --exercises-dir polyglot-benchmark --new
./benchmark/benchmark.py glm-latest-v2 --model glm-latest --edit-format whole --threads 10 --exercises-dir polyglot-benchmark --new
# Install GHC and Cabal
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
./benchmark/benchmark.py test-run --model openai/glm-latest --edit-format whole --threads 1 --num-tests 1 --languages haskell
./benchmark/benchmark.py single-test --model openai/glm-latest --edit-format whole --threads 1 --num-tests 1 --languages haskell --keywords hello-world --verbose
./benchmark/benchmark.py single-test --model glm-latest --edit-format whole --threads 1 --num-tests 1 --languages haskell --keywords hello-world --verbose
export OPENAI_API_KEY="sk-r6wUV-lbzCiTe3pQ1ktBew"
export OPENAI_API_BASE="https://grid.ai.juspay.net/v1"
./benchmark/benchmark.py hello-world-test --model glm-latest --keywords hello-world --num-tests 1 --languages haskell --edit-format whole --verbose
exit
tmux
exit
pip install -e .[dev]
/aider/benchmark/benchmark.py glm-latest-simple --model glm-latest --edit-format whole --threads 1 --num-tests 5 --exercises-dir polyglot-benchmark --new --languages haskell --keywords "difference,leap,hello,bob,two-fer"
# Reduce memory pressure
/aider/benchmark/benchmark.py glm-latest-simple --model glm-latest --edit-format whole --threads 1 --num-tests 1 --exercises-dir polyglot-benchmark --new --languages haskell --keywords "difference"
/aider/benchmark/benchmark.py glm-latest-simple --model glm-latest --edit-format whole --threads 1 --num-tests 5 --exercises-dir polyglot-benchmark --new --languages haskell
find tmp.benchmarks/2025-11-07-09-04-11--glm-latest-simple/ -name "*.aider.results.json"
python /aider/benchmark/benchmark.py glm-latest-simple-5tests --model glm-latest --edit-format whole --threads 1 --num-tests 5 --exercises-dir polyglot-benchmark --new --languages haskell
/aider/benchmark/benchmark.py glm-latest-simple-5tests --model glm-latest --edit-format whole --threads 1 --num-tests 5 --exercises-dir polyglot-benchmark --new --languages haskell --keywords "difference,hello,reverse,hamming,leap"
/aider/benchmark/benchmark.py glm-latest-simple-5tests --model glm-latest --edit-format whole --threads 1 --num-tests 5 --exercises-dir polyglot-benchmark --new --languages haskell
exit
/aider/benchmark/benchmark.py glm-latest-full-haskell --model glm-latest --edit-format whole --threads 1 --exercises-dir polyglot-benchmark --new --languages haskell
clear
./benchmark/benchmark.py o3-mini-run --model glm-latest --edit-format whole --threads 10 --num-tests 1 --exercises-dir polyglot-benchmark --new
RUN apt-get update && apt-get install -y     haskell-platform     cabal-install     ghc
apt-get update
apt-get install -y haskell-platform cabal-install ghc
cabal update
cabal --version
ghc --version
/aider/benchmark/benchmark.py glm-latest-haskell --model glm-latest --edit-format whole --threads 1 --exercises-dir polyglot-benchmark --new --languages haskell
/aider/benchmark/benchmark.py glm-latest-haskell --model glm-latest --edit-format whole --threads 4 --exercises-dir polyglot-benchmark --new --languages haskell
tmux
apt-get update && apt-get install -y tmux
/aider/benchmark/benchmark.py glm-latest-haskell --model glm-latest --edit-format whole --threads 4 --exercises-dir polyglot-benchmark --new --languages haskell
clear
/aider/benchmark/benchmark.py glm-latest-haskell --model glm-latest --edit-format whole --threads 4 --exercises-dir polyglot-benchmark --new --languages haskell
/aider/benchmark/benchmark.py glm-latest-haskell --model glm-latest --edit-format whole --threads 1 --exercises-dir polyglot-benchmark --new --languages haskell
/aider/benchmark/benchmark.py glm-latest-haskell --model glm-latest --edit-format whole --threads 4 --exercises-dir polyglot-benchmark --new --languages haskell
curl -X POST "https://grid.ai.juspay.net/v1/chat/completions"   -H "Content-Type: application/json"   -H "Authorization: Bearer sk-r6wUV-lbzCiTe3pQ1ktBew"   -d '{
    "model": "glm-latest",
    "messages": [
      {
        "role": "user",
        "content": "Hello, can you write a simple Haskell function?"
      }
    ],
    "max_tokens": 100,
    "temperature": 0.7
  }'
curl -X POST "https://grid.ai.juspay.net/v1/chat/completions"   -H "Content-Type: application/json"   -H "Authorization: Bearer sk-r6wUV-lbzCiTe3pQ1ktBew"   -d '{"model":"glm-latest","messages":[{"role":"user","content":"Test"}],"max_tokens":50}'
curl -X POST "https://grid.ai.juspay.net/v1/chat/completions"   -H "Content-Type: application/json"   -H "Authorization: Bearer sk-r6wUV-lbzCiTe3pQ1ktBew"   -d '{"model":"glm-latest","messages":[{"role":"user","content":"Test"}],"max_tokens":50}'   -s | jq .
curl -v -X POST "https://grid.ai.juspay.net/v1/chat/completions"   -H "Content-Type: application/json"   -H "Authorization: Bearer sk-r6wUV-lbzCiTe3pQ1ktBew"   -d '{"model":"glm-latest","messages":[{"role":"user","content":"Test"}],"max_tokens":50}'
curl -w "HTTP Status: %{http_code}\n"   -X POST "https://grid.ai.juspay.net/v1/chat/completions"   -H "Content-Type: application/json"   -H "Authorization: Bearer sk-r6wUV-lbzCiTe3pQ1ktBew"   -d '{"model":"glm-latest","messages":[{"role":"user","content":"Test"}],"max_tokens":50}'
curl -s -X POST "https://grid.ai.juspay.net/v1/chat/completions"   -H "Content-Type: application/json"   -H "Authorization: Bearer sk-r6wUV-lbzCiTe3pQ1ktBew"   -d '{"model":"glm-latest","messages":[{"role":"user","content":"Test"}],"max_tokens":50}' | cat
curl -s -X POST "https://grid.ai.juspay.net/v1/chat/completions"   -H "Content-Type: application/json"   -H "Authorization: Bearer sk-r6wUV-lbzCiTe3pQ1ktBew"   -d '{
    "model": "glm-latest",
    "messages": [
      {
        "role": "user", 
        "content": "What is 2+2? Please answer in one sentence."
      }
    ],
    "max_tokens": 50,
    "temperature": 0.1
  }' | jq -r '.choices[0].message.content'
curl -s -X POST "https://grid.ai.juspay.net/v1/chat/completions"   -H "Content-Type: application/json"   -H "Authorization: Bearer sk-r6wUV-lbzCiTe3pQ1ktBew"   -d '{"model":"glm-latest","messages":[{"role":"user","content":"What is 2+2?"}],"max_tokens":30}'
curl -s -X POST "https://grid.ai.juspay.net/v1/chat/completions"   -H "Content-Type: application/json"   -H "Authorization: Bearer sk-r6wUV-lbzCiTe3pQ1ktBew"   -d '{"model":"glm-latest","messages":[{"role":"user","content":"What is 2+2? Answer briefly."}],"max_tokens":100}' | python3 -m json.tool
/aider/benchmark/benchmark.py glm-latest-haskell --model glm-latest --edit-format whole --threads 4 --exercises-dir polyglot-benchmark --new --languages haskell
/aider/benchmark/benchmark.py glm-latest-haskell-single --model glm-latest --edit-format whole --threads 1 --exercises-dir polyglot-benchmark --new --languages haskell
apt-get remove --purge -y haskell-platform ghc cabal-install
apt-get autoremove -y
apt-get autoclean
rm -rf /root/.cabal /root/.ghc /usr/lib/ghc* /usr/share/doc/ghc*
apt-get update
apt-get install -y build-essential curl libffi-dev libgmp-dev
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
source ~/.bashrc
ghcup install ghc --set
ghcup install cabal --set
cabal update
./benchmark/benchmark.py o3-mini-run --model glm-latest --edit-format whole --threads 1 --num-tests 1 --exercises-dir polyglot-benchmark --new
exit
```bash
apt-get update && apt-get install -y tmux
```
###
# Start completely fresh container
docker run -it --memory=4g python:3.11-slim bash
apt-get update && apt-get install -y git curl
git clone https://github.com/MercuryTechnologies/haskell_llm_benchmark
cd haskell_llm_benchmark
pip install -r requirements.txt
pip install -e .
exit
exit
./benchmark/benchmark.py o3-mini-run --model glm-latest --edit-format whole --threads 1 --num-tests 1 --exercises-dir polyglot-benchmark --new
./benchmark/docker_build.sh
exit
pip install -e .[dev]
./benchmark/benchmark.py o3-mini-run --model glm-latest --edit-format whole --threads 1 --num-tests 1 --exercises-dir polyglot-benchmark --new
pip install -e .[dev]
./benchmark/benchmark.py o3-mini-run --model glm-latest --edit-format whole --threads 1 --num-tests 1 --exercises-dir polyglot-benchmark --new
docker run --rm -it aider-benchmark bash -c "cd /tmp && mkdir test && cd test && cabal init --simple test-project && echo 'main = putStrLn \"Hello\"' > Main.hs && cabal run"
./benchmark/benchmark.py o3-mini-run --model glm-latest --edit-format whole --threads 1 --num-tests 1 --exercises-dir polyglot-benchmark --new
./benchmark/benchmark.py glm-latest-4threads --model glm-latest --edit-format whole --threads 4 --exercises-dir polyglot-benchmark --new
./benchmark/benchmark.py o3-mini-run --model glm-latest --edit-format whole --threads 1 --num-tests 1 --exercises-dir polyglot-benchmark --new
./benchmark/benchmark.py glm-latest-4threads --model glm-latest --edit-format whole --threads 4 --exercises-dir polyglot-benchmark --new
./benchmark/benchmark.py glm-latest-4threads --model glm-latest --edit-format whole --threads 1 --exercises-dir polyglot-benchmark --new
exit
pip install -e .[dev]
./benchmark/benchmark.py o3-mini-run --model glm-latest --edit-format whole --threads 1 --num-tests 5 --exercises-dir polyglot-benchmark --new
exit
