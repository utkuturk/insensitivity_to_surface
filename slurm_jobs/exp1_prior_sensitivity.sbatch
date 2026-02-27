#!/bin/bash
#SBATCH --job-name=exp1_bf_sens
#SBATCH --output=/home/uturk_umass_edu/insensitivity_to_surface/slurm_jobs/logs/exp1_bf_sens_%j.out
#SBATCH --error=/home/uturk_umass_edu/insensitivity_to_surface/slurm_jobs/logs/exp1_bf_sens_%j.err
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

MODEL_IDS="${MODEL_IDS:-7 8 9}"
THREADS="${THREADS:-7}"
CHAINS="${CHAINS:-4}"
CORES="${CORES:-4}"
ITER="${ITER:-24000}"
WARMUP="${WARMUP:-6000}"
SEED="${SEED:-25022026}"
FORCE_REFIT="${FORCE_REFIT:-true}"
ADAPT_DELTA="${ADAPT_DELTA:-0.999}"
MAX_TREEDEPTH="${MAX_TREEDEPTH:-15}"
RUN_BRIDGE="${RUN_BRIDGE:-false}"
BRIDGE_FORCE="${BRIDGE_FORCE:-false}"
BRIDGE_METHOD="${BRIDGE_METHOD:-normal}"
BRIDGE_REPETITIONS="${BRIDGE_REPETITIONS:-1}"
BRIDGE_MAXITER="${BRIDGE_MAXITER:-2000}"
BRIDGE_USE_NEFF="${BRIDGE_USE_NEFF:-true}"

for MODEL_ID in ${MODEL_IDS}; do
  echo "[$(date)] Starting model ${MODEL_ID}"
  Rscript --no-restore --no-save "${PROJECT_DIR}/utility/slurm/run_exp1_prior_sensitivity.R" \
    --model-id "${MODEL_ID}" \
    --threads "${THREADS}" \
    --chains "${CHAINS}" \
    --cores "${CORES}" \
    --iter "${ITER}" \
    --warmup "${WARMUP}" \
    --seed "${SEED}" \
    --force "${FORCE_REFIT}" \
    --adapt-delta "${ADAPT_DELTA}" \
    --max-treedepth "${MAX_TREEDEPTH}"
  echo "[$(date)] Finished model ${MODEL_ID}"
done

if [ "${RUN_BRIDGE}" = "true" ]; then
  echo "[$(date)] Starting Exp1 bridge BF computations"
  Rscript --no-restore --no-save "${PROJECT_DIR}/utility/slurm/compute_exp1_bridge_bf.R" \
    --force-bridge "${BRIDGE_FORCE}" \
    --bridge-method "${BRIDGE_METHOD}" \
    --bridge-repetitions "${BRIDGE_REPETITIONS}" \
    --bridge-maxiter "${BRIDGE_MAXITER}" \
    --bridge-use-neff "${BRIDGE_USE_NEFF}"
  echo "[$(date)] Finished Exp1 bridge BF computations"
else
  echo "[$(date)] Skipping Exp1 bridge BF computations (RUN_BRIDGE=${RUN_BRIDGE})"
fi
