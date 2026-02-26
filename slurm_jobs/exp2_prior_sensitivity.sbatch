#!/bin/bash
#SBATCH --job-name=exp2_bf_sens
#SBATCH --output=slurm_jobs/logs/exp2_bf_sens_%j.out
#SBATCH --error=slurm_jobs/logs/exp2_bf_sens_%j.err
#SBATCH --time=48:00:00
#SBATCH --partition=cpu
#SBATCH --mem=64G
#SBATCH --cpus-per-task=28

set -euo pipefail

module load r-rocker-ml-verse/4.5.1_cuda12.8.1+apptainer

cd "${SLURM_SUBMIT_DIR}"
mkdir -p slurm_jobs/logs

MODEL_IDS="${MODEL_IDS:-1 2 3 4 5 6 7 8 9}"

for MODEL_ID in ${MODEL_IDS}; do
  echo "[$(date)] Starting Exp2 model ${MODEL_ID}"
  Rscript --no-restore --no-save utility/slurm/run_exp2_prior_sensitivity.R \
    --model-id "${MODEL_ID}" \
    --threads 7 \
    --chains 4 \
    --cores 4 \
    --iter 12000 \
    --warmup 2000 \
    --seed 25022026
  echo "[$(date)] Finished Exp2 model ${MODEL_ID}"
done

echo "[$(date)] Starting Exp2 bridge BF computations"
Rscript --no-restore --no-save utility/slurm/compute_exp2_bridge_bf.R
echo "[$(date)] Finished Exp2 bridge BF computations"
