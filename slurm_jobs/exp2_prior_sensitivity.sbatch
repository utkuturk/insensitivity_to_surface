#!/bin/bash
#SBATCH --job-name=exp2_bf_sens
#SBATCH --output=/home/uturk_umass_edu/insensitivity_to_surface/slurm_jobs/logs/exp2_bf_sens_%j.out
#SBATCH --error=/home/uturk_umass_edu/insensitivity_to_surface/slurm_jobs/logs/exp2_bf_sens_%j.err
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

MODEL_IDS="${MODEL_IDS:-1 2 3 4 5 6 7 8 9}"
THREADS="${THREADS:-7}"
CHAINS="${CHAINS:-4}"
CORES="${CORES:-4}"
ITER="${ITER:-12000}"
WARMUP="${WARMUP:-2000}"
SEED="${SEED:-25022026}"
FORCE_REFIT="${FORCE_REFIT:-false}"
RUN_BRIDGE="${RUN_BRIDGE:-true}"

for MODEL_ID in ${MODEL_IDS}; do
  echo "[$(date)] Starting Exp2 model ${MODEL_ID}"
  Rscript --no-restore --no-save "${PROJECT_DIR}/utility/slurm/run_exp2_prior_sensitivity.R" \
    --model-id "${MODEL_ID}" \
    --threads "${THREADS}" \
    --chains "${CHAINS}" \
    --cores "${CORES}" \
    --iter "${ITER}" \
    --warmup "${WARMUP}" \
    --seed "${SEED}" \
    --force "${FORCE_REFIT}"
  echo "[$(date)] Finished Exp2 model ${MODEL_ID}"
done

if [ "${RUN_BRIDGE}" = "true" ]; then
  echo "[$(date)] Starting Exp2 bridge BF computations"
  Rscript --no-restore --no-save "${PROJECT_DIR}/utility/slurm/compute_exp2_bridge_bf.R"
  echo "[$(date)] Finished Exp2 bridge BF computations"
else
  echo "[$(date)] Skipping Exp2 bridge BF computations (RUN_BRIDGE=${RUN_BRIDGE})"
fi
