

# from Remote -------------------------------------------------------------

# Copy files to the server

file.copy("C:/Users/admin/switchdrive/4 irt/simpackage_0.0.0.9000.tar.gz", "Y:/simpackage_0.0.0.9000.tar.gz", overwrite = T)

file.copy("C:/Users/admin/switchdrive/4 irt/paper 2/simulationstudy_anova.R", "Y:/simulationstudy_anova.R", overwrite = T)

file.copy("C:/Users/admin/switchdrive/4 irt/paper 2/simulationstudy.R", "Y:/simulationstudy1.R", overwrite = T)

file.copy("C:/Users/admin/switchdrive/4 irt/paper 2/simulationstudy_2dmixed.R", "Y:/simulationstudy_2dmixed.R", overwrite = T)

file.copy("C:/Users/admin/switchdrive/4 irt/paper 2/fun6_sim_2.R", "Y:/fun6_sim_2.R", overwrite = T)


# file.copy("C:/Users/admin/switchdrive/4 irt/paper 2/fun3_sim.R", "Y:/fun3_sim.R", overwrite = T)
# file.copy("C:/Users/admin/switchdrive/4 irt/paper 2/fun4_sim.R", "Y:/fun4_sim.R", overwrite = T)
# file.copy("C:/Users/admin/switchdrive/4 irt/paper 2/fun5_sim.R", "Y:/fun5_sim.R", overwrite = T)
# file.copy("C:/Users/admin/switchdrive/4 irt/paper 2/fun6_sim.R", "Y:/fun6_sim.R", overwrite = T)


nohup nice -n 11 Rscript fun6_sim_2.R > run1.out &

nohup nice -n 11 Rscript simulationstudy_2dmixed.R > run1.out &

nohup nice -n 11 Rscript simulationstudy_dummy.R > run1.out &


nohup nice -n 11 Rscript simulationstudy_anova.R > run1.out &

nohup nice -n 11 Rscript simulationstudy1.R > run1.out &


pkill -u fzimmer


# science cloud -----------------------------------------------------------

# launch instance with volume
# connect to the instance: https://github.com/naturalis/openstack-docs/wiki/Howto:-Creating-and-using-OpenStack-SSH-keypairs-on-Windows
# connect to the drive: https://github.com/naturalis/openstack-docs/wiki/Howto:-Copying-files-to-your-OpenStack-instance-on-Windows


# install current R and other stuff
sudo apt-get update
sudo apt install dirmngr gnupg apt-transport-https ca-certificates software-properties-common

sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9
sudo add-apt-repository 'deb https://cloud.r-project.org/bin/linux/ubuntu focal-cran40/'

sudo apt install r-base

sudo apt-get install libcurl4-openssl-dev libxml2-dev
sudo apt install build-essential checkinstall zlib1g-dev -y

sudo apt install cmake

# Install missing packages (packages object from helper.R)
install.packages(setdiff(packages, rownames(installed.packages())))


# Mount Volume
lsblk # check if needed


sudo mkfs.ext4 /dev/vdb
sudo mount /dev/vdb /mnt
sudo chown -R ubuntu:ubuntu /mnt





