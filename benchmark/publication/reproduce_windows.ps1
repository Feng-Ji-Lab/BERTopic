[CmdletBinding()]
param(
    [Parameter(Mandatory=$true)][string]$PythonPath,
    [string]$OutputRoot = '',
    [switch]$ValidateOnly
)
$ErrorActionPreference = 'Stop'
if ($env:OS -ne 'Windows_NT') { throw 'This reproduction script targets Windows.' }
$repoPath = (Get-Item -LiteralPath $PSScriptRoot).Parent.Parent.FullName
$pythonPathResolved = (Resolve-Path -LiteralPath $PythonPath).Path
$rscriptPath = (Get-Command Rscript.exe -ErrorAction Stop).Source
$rCommandPath = (Get-Command R.exe -ErrorAction Stop).Source
function Invoke-Checked {
    param([string]$Program, [string[]]$Arguments)
    & $Program @Arguments
    if ($LASTEXITCODE -ne 0) { throw "Command failed with exit code $LASTEXITCODE ($Program)" }
}
Push-Location -LiteralPath $repoPath
$previousLibrary = $env:R_LIBS
try {
    Invoke-Checked -Program $pythonPathResolved -Arguments @('benchmark/verify_archive.py')
    Invoke-Checked -Program $pythonPathResolved -Arguments @('-c', "import sys; from importlib.metadata import version; assert sys.version_info[:2] == (3,10), sys.version; assert version('bertopic') == '0.16.0'; print('PASS: Python 3.10 / BERTopic 0.16.0')")
    Invoke-Checked -Program $rscriptPath -Arguments @('-e', "stopifnot(as.character(getRversion())=='4.4.1'); for(p in c('reticulate','rlang','tibble','testthat','withr','Matrix','jsonlite','htmltools','ggplot2')) if(!requireNamespace(p,quietly=TRUE)) stop(paste('Missing R package:',p)); cat('PASS: R 4.4.1 and required packages\n')")
    $releaseCommit = & git rev-parse 'v0.1.2^{commit}'
    if ($LASTEXITCODE -ne 0 -or $releaseCommit -ne 'faee106360389a8e75199893f6e8c68c28876c8d') { throw 'Clone SOURCE.bundle or the exact Git checkout before rerunning.' }
    if ($ValidateOnly) { Write-Output 'PASS: archive and Windows reproduction preflight'; return }
    if (!$OutputRoot) { $OutputRoot = Join-Path (Split-Path -Parent $repoPath) 'bertopic-reproduction-0.1.2' }
    $outputPath = [IO.Path]::GetFullPath($OutputRoot)
    if (Test-Path -LiteralPath $outputPath) { throw 'OutputRoot already exists; choose a new directory.' }
    New-Item -ItemType Directory -Path $outputPath | Out-Null
    $libraryPath = Join-Path $outputPath 'r-library'
    New-Item -ItemType Directory -Path $libraryPath | Out-Null
    $env:R_LIBS = $libraryPath
    $sourceArchive = Join-Path $repoPath 'provenance/releases/BERTopic_0.1.2.tar.gz'
    Invoke-Checked -Program $rCommandPath -Arguments @('CMD', 'INSTALL', "--library=$libraryPath", $sourceArchive)
    $resultPath = Join-Path $outputPath 'results'
    $examplePath = Join-Path $outputPath 'example-results'
    Invoke-Checked -Program $rscriptPath -Arguments @('benchmark/run_benchmark.R', '--python', $pythonPathResolved,
        '--documents', 'data/sms_spam.csv', '--embeddings', 'benchmark/inputs/reduced_embeddings.npy',
        '--output', $resultPath, '--repetitions', '5', '--max-docs', '2247', '--seed', '42',
        '--min-cluster-size', '10', '--package-mode', 'installed', '--package-archive', $sourceArchive,
        '--release-tag', 'v0.1.2', '--expected-package-version', '0.1.2', '--expected-bertopic-version', '0.16.0')
    Invoke-Checked -Program $rscriptPath -Arguments @('benchmark/export_artifacts.R', '--results', $resultPath)
    Invoke-Checked -Program $rscriptPath -Arguments @('benchmark/run_example.R', '--python', $pythonPathResolved,
        '--package-archive', $sourceArchive, '--output', $examplePath, '--seed', '42',
        '--max-docs', '2247', '--model-revision', '1110a243fdf4706b3f48f1d95db1a4f5529b4d41')
    Write-Output "PASS: reproduced outputs saved to $outputPath"
}
finally {
    $env:R_LIBS = $previousLibrary
    Pop-Location
}
