cul=`pwd`
cd $cul
rm sub_allruns.sh
echo -e "#!/bin/bash\n#SBATCH -N 48\n#SBATCH --ntasks-per-node=1\n#SBATCH --ntasks=48\n#SBATCH --cpus­per­task=12">$cul/sub_allruns.sh
ls -d Run* | while read runs;
do
	cd $runs
	if [ ! -e result ] ; then
		mkdir result 
	fi
	
	ls -d JOB_* | while read job1;
	do
		cd $job1
		cp $cul/Barley_KeLiu.xml .
		apsimfile=`ls *.apsim`
		station=${apsimfile:0:4}
		wpath=`pwd`
		#rm run_${station}.sh
		echo -e "#!/bin/bash\nmodule load apps/singularity/3.6.1\nmodule load compiler/devtoolset/7.3.1\nmodule load compiler/rocm/2.9\nmodule load mpi/hpcx/2.4.1/gcc-7.3.1\ncd ${wpath}\n">run_${station}.sh
		#echo -e "#!/bin/bash\nsrun -N 1 -c 12 sh ${wpath}/run_${station}.sh">sub_run_${station}.sh
		echo -e "cd ${wpath}/">>$cul/sub_allruns.sh
		echo -e "srun -N1 --ntasks=1 sh ${wpath}/run_${station}.sh &">>$cul/sub_allruns.sh
		#echo -e "sbatch -p normal ${wpath}/sub_run_${station}.sh">>$cul/sub_allruns.sh
		ls ${wpath}/../MET/${station}_* | while read metfile;
		do 
			metfile1=`basename ${metfile}`
			echo -e "rm MET.MET\n ln -s ${metfile} ./MET.MET\n " >>run_${station}.sh
			echo -e "singularity exec /public/home/zzuaga06/tools/SIngularity_Container/apsim7.9-oxdef.sif mono /usr/local/APSIM/Model/Apsim.exe ${wpath}/$apsimfile\n" >>run_${station}.sh
			echo -e "mv ${wpath}/S1_VS.out ${wpath}/../result/${metfile1%.MET*}_S1_VS.csv\n" >>run_${station}.sh
			echo -e "mv ${wpath}/S1_VT.out ${wpath}/../result/${metfile1%.MET*}_S1_VT.csv\n" >>run_${station}.sh
			echo -e "mv ${wpath}/S2_VS.out ${wpath}/../result/${metfile1%.MET*}_S2_VS.csv\n" >>run_${station}.sh
			echo -e "mv ${wpath}/S2_VT.out ${wpath}/../result/${metfile1%.MET*}_S2_VT.csv\n" >>run_${station}.sh
			echo -e "mv ${wpath}/S3_VS.out ${wpath}/../result/${metfile1%.MET*}_S3_VS.csv\n" >>run_${station}.sh
			echo -e "mv ${wpath}/S3_VT.out ${wpath}/../result/${metfile1%.MET*}_S3_VT.csv\n" >>run_${station}.sh
			echo -e "rm ${wpath}/S[1-3]*.out\n" >>run_${station}.sh
		done
		cd ..
	done
	cd $cul
done
echo -e "wait">>$cul/sub_allruns.sh
