#!/bin/bash
#SBATCH --job-name=exp1_verbal_bf
#SBATCH --output=/home/uturk_umass_edu/insensitivity_to_surface/slurm_jobs/logs/exp1_verbal_bf_%j.out
#SBATCH --error=/home/uturk_umass_edu/insensitivity_to_surface/slurm_jobs/logs/exp1_verbal_bf_%j.err
#SBATCH --time=48:00:00
#SBATCH --partition=cpu
#SBATCH --mem=64G
#SBATCH --cpus-per-task=28
#SBATCH --chdir=/home/uturk_umass_edu/insensitivity_to_surface

set -euo pipefail

module load r-rocker-ml-verse/4.5.1_cuda12.8.1+apptainer

PROJECT_DIR="/home/uturk_umass_edu/insensitivity_to_surface"
export PROJECT_DIR
cd "${PROJECT_DIR}"
mkdir -p slurm_jobs/logs

MODEL_IDS="${MODEL_IDS:-1 2 3 4 5 6 7 8 9 10}"
THREADS="${THREADS:-7}"
CHAINS="${CHAINS:-4}"
CORES="${CORES:-4}"
ITER="${ITER:-12000}"
WARMUP="${WARMUP:-2000}"
SEED="${SEED:-25022026}"
FORCE_REFIT="${FORCE_REFIT:-false}"
ADAPT_DELTA="${ADAPT_DELTA:-}"
MAX_TREEDEPTH="${MAX_TREEDEPTH:-}"
RUN_BRIDGE="${RUN_BRIDGE:-true}"
BRIDGE_FORCE="${BRIDGE_FORCE:-false}"
BRIDGE_METHOD="${BRIDGE_METHOD:-normal}"
BRIDGE_REPETITIONS="${BRIDGE_REPETITIONS:-1}"
BRIDGE_MAXITER="${BRIDGE_MAXITER:-2000}"
BRIDGE_USE_NEFF="${BRIDGE_USE_NEFF:-true}"

for MODEL_ID in ${MODEL_IDS}; do
  echo "[$(date)] Starting RC-only model ${MODEL_ID}"
  Rscript --no-restore --no-save "${PROJECT_DIR}/utility/slurm/run_exp1_verbal_prior_sensitivity.R" \
    --model-id "${MODEL_ID}" \
    --threads "${THREADS}" \
    --chains "${CHAINS}" \
    --cores "${CORES}" \
    --iter "${ITER}" \
    --warmup "${WARMUP}" \
    --seed "${SEED}" \
    --force "${FORCE_REFIT}" \
    ${ADAPT_DELTA:+--adapt-delta "${ADAPT_DELTA}"} \
    ${MAX_TREEDEPTH:+--max-treedepth "${MAX_TREEDEPTH}"}
  echo "[$(date)] Finished RC-only model ${MODEL_ID}"
done

if [ "${RUN_BRIDGE}" = "true" ]; then
  echo "[$(date)] Starting RC-only bridge BF computations"
  Rscript --no-restore --no-save "${PROJECT_DIR}/utility/slurm/compute_exp1_verbal_bridge_bf.R" \
    --force-bridge "${BRIDGE_FORCE}" \
    --bridge-method "${BRIDGE_METHOD}" \
    --bridge-repetitions "${BRIDGE_REPETITIONS}" \
    --bridge-maxiter "${BRIDGE_MAXITER}" \
    --bridge-use-neff "${BRIDGE_USE_NEFF}"
  echo "[$(date)] Finished RC-only bridge BF computations"
else
  echo "[$(date)] Skipping RC-only bridge BF computations (RUN_BRIDGE=${RUN_BRIDGE})"
fi
