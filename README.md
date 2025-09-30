# Haskell LLM Benchmark

This is a test harness to evaluate LLM models on their ability to consistently follow instructions to succesfully edit Haskell code. 

It is a modified version of the [Aider benchmark harness](https://github.com/Aider-AI/aider/blob/main/benchmark/README.md) adapted to include a Haskell environment.

The benchmark is based on [Exercism's Haskell exercises](https://exercism.org/tracks/haskell) ([Github](https://github.com/exercism/haskell)). This benchmark evaluates how effectively a coding assistant and LLMs can translate a natural language coding request into executable code saved into files that pass unit tests. It provides an end-to-end evaluation of not just the LLM's coding ability, but also its capacity to edit existing code and format those code edits so that aider can save the edits to the local source files.

_Last updated: 2025-09-29_

![Haskell LLM Benchmark](/benchmark-result/report-2025-09-29-23-50-54/benchmark_comparison.png)

| Model | Tests | Pass % | Pass 1st Try % | Tests Passed | Passes 1st Try | Well Formed % | Errors | Sec/Test | Total Cost ($) | Cost/Test ($) |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| gpt-5-high | 112 | 90.2 | 82.1 | 101 | 92 | 100.0 | 0 | 117.2 | 6.93 | 0.0619 |
| o3-high | 112 | 88.4 | 73.2 | 99 | 82 | 100.0 | 0 | 51.7 | 19.05 | 0.1701 |
| gpt-5-2025-08-07 | 112 | 88.4 | 79.5 | 99 | 89 | 100.0 | 0 | 56.1 | 3.79 | 0.0339 |
| o3 | 112 | 84.8 | 73.2 | 95 | 82 | 100.0 | 0 | 27.2 | 11.81 | 0.1055 |
| o3-pro | 112 | 84.8 | 74.1 | 95 | 83 | 99.1 | 3 | 205.9 | 0.00 | 0.0000 |
| gemini-2.5-pro-preview | 112 | 81.2 | 76.8 | 91 | 86 | 99.1 | 6 | 64.3 | 0.00 | 0.0000 |
| claude-opus-4-20250514 | 112 | 81.2 | 65.2 | 91 | 73 | 100.0 | 0 | 22.5 | 0.00 | 0.0000 |
| claude-sonnet-4-5-20250929 | 112 | 79.5 | 67.0 | 89 | 75 | 100.0 | 0 | 13.9 | 2.48 | 0.0222 |
| claude-sonnet-4-20250514 | 112 | 77.7 | 61.6 | 87 | 69 | 99.1 | 4 | 14.8 | 0.00 | 0.0000 |
| o3-mini | 112 | 75.0 | 63.4 | 84 | 71 | 100.0 | 0 | 37.5 | 2.13 | 0.0190 |
| o4-mini | 112 | 74.1 | 67.9 | 83 | 76 | 99.1 | 1 | 29.4 | 1.81 | 0.0162 |
| claude-opus-4-1-20250805 | 112 | 69.6 | 59.8 | 78 | 67 | 100.0 | 1 | 21.4 | 13.28 | 0.1186 |
| x-ai/grok-code-fast-1 | 112 | 68.8 | 51.8 | 77 | 58 | 100.0 | 4 | 23.1 | 0.00 | 0.0000 |
| qwen/qwen3-coder | 112 | 66.1 | 50.0 | 74 | 56 | 99.1 | 1 | 31.3 | 0.00 | 0.0000 |
| gpt-4.1-2025-04-14 | 112 | 65.2 | 57.1 | 73 | 64 | 100.0 | 0 | 7.6 | 1.14 | 0.0102 |
| deepseek-chat-v3.1 | 112 | 65.2 | 48.2 | 73 | 54 | 100.0 | 0 | 47.8 | 0.00 | 0.0000 |
| gpt-4.1-mini-2025-04-14 | 112 | 63.4 | 51.8 | 71 | 58 | 100.0 | 0 | 5.3 | 0.24 | 0.0021 |
| qwen/qwen3-max | 112 | 59.8 | 53.6 | 67 | 60 | 100.0 | 705 | 66.5 | 0.00 | 0.0000 |






___

## Instructions

Can generally follow the instructions in the [Aider benchmark harness](https://github.com/Aider-AI/aider/blob/main/benchmark/README.md); with the following exceptions:

- clone this repo
- exercises are included in the `tmp.benchmarks` directory, no need to clone the exercises (although you are welcome to contribute new ones)

On my macOS machine, running the benchmark in Docker would consistently fail with some heap corruption error ([issue](https://github.com/Aider-AI/aider/issues/3718)). A nix environment is provided although you probably want to run this in a safe environment like a VM (the benchmark runs code produced by an LLM so it's important to run it in an isolated environment).

Once you have a cloned repo:

```sh
nix-develop

# set your API keys (alternatively, you can set the keys in .envrc if using direnv (nix env has it set up))
export OPENAI_API_KEY=sk-proj-...
export ANTHROPIC_API_KEY=...
export GEMINI_API_KEY=...

# run the benchmark (try a single exercise first)
./benchmark/benchmark.py o3-mini-run --model o3-mini --edit-format whole --threads 10 --num-tests 1 --exercises-dir polyglot-benchmark --new

./benchmark/benchmark.py o3-mini-full-run --model o3-mini --edit-format whole --threads 10 --exercises-dir polyglot-benchmark --new

# for sonnet thinking
./benchmark/benchmark.py claude-3-7-thinking-full-run-final --model anthropic/claude-3-7-sonnet-20250219 --edit-format whole --threads 5 --exercises-dir polyglot-benchmark --new --read-model-settings .aider.model.settings.yml
```

You need to be mindful of the API limits of the model you are using. For high volume APIs (e.g. OpenAI), I've had success using `20` threads. For Anthropic, I've had success using `5` threads, etc...

Reference for model providers and models: https://aider.chat/docs/llms.html

### Generating Reports

After running benchmarks for one or more models, you can generate comparison reports with:

```sh
# Generate reports for all benchmarks (automatically uses all folders in tmp.benchmarks except polyglot-benchmark)
./benchmark/summarize_benchmark.py

# Generate reports for specific benchmark directories
./benchmark/summarize_benchmark.py path/to/dir1 path/to/dir2

# Specify custom output paths
./benchmark/summarize_benchmark.py --table-output custom_table.csv --plot-output custom_plot.png
```

The report generator will:
- Extract key metrics from all benchmark results
- Format model names for better readability
- Sort models by pass rate
- Generate a formatted table in both CSV and Markdown formats
- Create a visual comparison chart showing pass rates and costs
- Save results in a timestamped directory under benchmark-result/

___ 

### Updating to latest aider version

```sh
git fetch upstream
git merge upstream/main
```
