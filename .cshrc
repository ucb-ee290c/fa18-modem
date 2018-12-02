# Aliases
#alias ls='ls -aF'
#alias rm="rm -i"

# Prompt
#PS1="{\u@\h:\w}\n\t $ "

#source /tools/support/lsf/conf/profile.lsf
source /tools/flexlm/flexlm.cshrc

# Environment
setenv RIGGE_HOME /users/rigge
setenv MHOME /users/meng_wei

#export TERM=xterm-color
#export FTP_PASSIVE_MODE=yes
#export PATH=$PATH:~/bin


#

#export PATH=$HOME/.local/gradle-2.4/bin:$PATH

setenv PATH $RIGGE_HOME/.local/gradle-2.4/bin:$PATH

#export PATH=$HOME/gcc-7.3.0/bin:$PATH
setenv PATH $RIGGE_HOME/gcc-7.3.0/bin:$PATH
#export PATH=$HOME/bin:$PATH
setenv PATH $RIGGE_HOME/bin:$PATH
#export PATH=$HOME/.local/bin:$PATH
setenv PATH $RIGGE_HOME/.local/bin:$PATH

setenv PATH $MHOME/automake-1.15/bin:$PATH
setenv PATH $MHOME/libtool-2.4.6:$PATH
setenv PATH $MHOME/pkg-config-0.29.2:$PATH


# export LD_LIBRARY_PATH=$HOME/riscv2/lib64:$HOME/riscv2/lib:$LD_LIBRARY_PATH

#export LD_LIBRARY_PATH=$HOME/gcc-7.3.0/lib64:$HOME/gcc-7.3.0/lib:$LD_LIBRARY_PATH
#export LD_LIBRARY_PATH=$HOME/.local/lib64:$HOME/.local/lib:$LD_LIBRARY_PATH

setenv LD_LIBRARY_PATH $RIGGE_HOME/gcc-7.3.0/lib64:$RIGGE_HOME/gcc-7.3.0/lib:$LD_LIBRARY_PATH
setenv LD_LIBRARY_PATH $RIGGE_HOME/.local/lib64:$RIGGE_HOME/.local/lib:$LD_LIBRARY_PATH
#setenv LD_LIBRARY_PATH $MHOME/gcc-7.3.0/lib64:$RIGGE_HOME/gcc-7.3.0/lib:$LD_LIBRARY_PATH

#java

setenv MY_JDK jdk1.8.0_144
setenv JAVA_HOME /users/rigge/$MY_JDK/
setenv PATH /users/rigge/$MY_JDK/bin:$PATH
setenv LD_LIBRARY_PATH /users/rigge/$MY_JDK/lib:$LD_LIBRARY_PATH
setenv MANPATH /users/rigge/$MY_JDK/man:$MANPATH

#setenv RISCV $TOP/riscv

#setenv PATH $RISCV/bin:$PATH

setenv RISCV /tools/projects/dunn/riscv-tools
setenv PATH $RISCV/bin:$PATH
setenv LD_LIBRARY_PATH $RISCV/lib64:$RISCV/lib:$LD_LIBRARY_PATH



