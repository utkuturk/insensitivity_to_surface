#!/bin/bash
#SBATCH --job-name=exp1_wide_bf
#SBATCH --output=/home/uturk_umass_edu/insensitivity_to_surface/slurm_jobs/logs/exp1_wide_bf_%j.out
#SBATCH --error=/home/uturk_umass_edu/insensitivity_to_surface/slurm_jobs/logs/exp1_wide_bf_%j.err
#SBATCH --time=48:00:00
#SBATCH --partition=cpu
#SBATCH --mem=96G
#SBATCH --cpus-per-task=28
#SBATCH --chdir=/home/uturk_umass_edu/insensitivity_to_surface

set -euo pipefail

module load r-rocker-ml-verse/4.5.1_cuda12.8.1+apptainer

PROJECT_DIR="/home/uturk_umass_edu/insensitivity_to_surface"
export PROJECT_DIR
cd "${PROJECT_DIR}"
mkdir -p slurm_jobs/logs

RECOMPILE="${RECOMPILE:-false}"
BRIDGE_FORCE="${BRIDGE_FORCE:-false}"
BRIDGE_METHOD="${BRIDGE_METHOD:-normal}"
BRIDGE_REPETITIONS="${BRIDGE_REPETITIONS:-1}"
BRIDGE_MAXITER="${BRIDGE_MAXITER:-2000}"
BRIDGE_USE_NEFF="${BRIDGE_USE_NEFF:-true}"
SAVE_MERGED_CACHE="${SAVE_MERGED_CACHE:-true}"
OVERWRITE_MAIN_CACHE="${OVERWRITE_MAIN_CACHE:-false}"

echo "[$(date)] Starting Exp1 Wide bridge BF computations"
Rscript --no-restore --no-save "${PROJECT_DIR}/utility/slurm/compute_exp1_wide_bridge_bf.R" \
  --recompile "${RECOMPILE}" \
  --force-bridge "${BRIDGE_FORCE}" \
  --bridge-method "${BRIDGE_METHOD}" \
  --bridge-repetitions "${BRIDGE_REPETITIONS}" \
  --bridge-maxiter "${BRIDGE_MAXITER}" \
  --bridge-use-neff "${BRIDGE_USE_NEFF}" \
  --save-merged-cache "${SAVE_MERGED_CACHE}" \
  --overwrite-main-cache "${OVERWRITE_MAIN_CACHE}"
echo "[$(date)] Finished Exp1 Wide bridge BF computations"
