export EDITOR=/usr/bin/vim
export GTK2_RC_FILES="$HOME/.gtkrc-2.0"

export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS="@im=fcitx"
export PATH=$HOME/.cargo/bin:$HOME/go/bin:$PATH
export PATH=$HOME/anaconda3/bin:$PATH
export PATH=$HOME/.local/bin:$PATH
export PATH=$HOME/projects/dart/flutter/bin:$PATH
export LGOPATH=$HOME/projects/go
export GOPATH=$HOME/projects/go
export WINIT_HIDPI_FACTOR=2.5
export GOPROXY=https://athens.azurefd.net/

alias amap='docker run -it --rm -w /data -v $(pwd):/data booyaabes/kali-linux-full amap'
alias burpsuite='docker run -it --rm -e DISPLAY -v $HOME/.Xauthority:/root/.Xauthority --net=host booyaabes/kali-linux-full java -jar /usr/bin/burpsuite'
alias commix='docker run -it --rm -w /data -v $(pwd):/data booyaabes/kali-linux-full commix'
alias dirb='docker run -it --rm -w /data -v $(pwd):/data booyaabes/kali-linux-full dirb'
alias dnschef='docker run -it --rm -w /data -v $(pwd):/data --net=host booyaabes/kali-linux-full dnschef'
alias dnsenum='docker run -it --rm -w /data -v $(pwd):/data booyaabes/kali-linux-full dnsenum'
alias dnsmap='docker run -it --rm -w /data -v $(pwd):/data booyaabes/kali-linux-full dnsmap'
alias dnsrecon='docker run -it --rm -w /data -v $(pwd):/data booyaabes/kali-linux-full dnsrecon'
alias dnswalk='docker run -it --rm booyaabes/kali-linux-full dnswalk'
alias hping3='docker run -it --rm -w /data -v $(pwd):/data booyaabes/kali-linux-full hping3'
alias kali-sh='docker run -it --rm --net=host --privileged -e DISPLAY -v $HOME/.Xauthority:/root/.Xauthority booyaabes/kali-linux-full /bin/bash'
alias msfconsole='docker run -it --rm --net=host  -v ~/.msf4:/root/.msf4 -v /tmp/msf:/tmp/data booyaabes/kali-linux-full msfconsole'
alias msfvenom='docker run -it --rm -w /data -v $(pwd):/data booyaabes/kali-linux-full msfvenom'
alias ndiff='docker run -it --rm -w /data -v $(pwd):/data booyaabes/kali-linux-full ndiff'
alias netdiscover='docker run -it --rm --net=host -w /data -v $(pwd):/data booyaabes/kali-linux-full netdiscover'
alias nikto='docker run -it --rm --net=host -w /data -v $(pwd):/data booyaabes/kali-linux-full nikto'
alias nmap='docker run --rm --net=host --privileged booyaabes/kali-linux-full nmap'
alias padbuster='docker run -it --rm booyaabes/kali-linux-full padbuster'
alias reaver='docker run -it --rm --net=host --privileged -w /data -v $(pwd):/data booyaabes/kali-linux-full reaver'
alias responder='docker run -it --rm --net=host booyaabes/kali-linux-full responder'
alias searchsploit='docker run --rm booyaabes/kali-linux-full searchsploit'
alias sqlmap='docker run -it --rm --net=host -w /data -v ~/.sqlmap:/root/.sqlmap -v $(pwd):/data booyaabes/kali-linux-full sqlmap'
alias socat='docker run --rm -w /data -v $(pwd):/data --net=host booyaabes/kali-linux-full socat'
alias tshark='docker run --rm -w /data -v $(pwd):/data --net=host --privileged booyaabes/kali-linux-full tshark'
alias volafox='docker run -it --rm -w /data -v $(pwd):/data booyaabes/kali-linux-full volafox'
alias volatility='docker run -it --rm -w /data -v $(pwd):/data booyaabes/kali-linux-full volatility'
alias wash='docker run -it --rm -w /data -v $(pwd):/data --net=host --privileged booyaabes/kali-linux-full wash'
alias webscarab='docker run -it --rm -w /data -v $(pwd):/data -e DISPLAY -v $HOME/.Xauthority:/root/.Xauthority --net=host booyaabes/kali-linux-full java -jar /usr/bin/webscarab'
alias wireshark='docker run -it --rm -w /data -v $(pwd):/data -e DISPLAY -v $HOME/.Xauthority:/root/.Xauthority --net=host --privileged booyaabes/kali-linux-full wireshark'
alias wpscan='docker run -it --rm -w /data -v $(pwd):/data booyaabes/kali-linux-full wpscan'
alias yersinia='docker run -it --rm -w /data -v $(pwd):/data booyaabes/kali-linux-full yersinia'
alias zaproxy='docker run -it --rm -v ~/.ZAP:/root/.ZAP -e DISPLAY -v $HOME/.Xauthority:/root/.Xauthority --net=host booyaabes/kali-linux-full zaproxy'

export PATH=/home/vitalyr/projects/go/bin:/home/vitalyr/.deno/bin:$PATH

export PUB_HOSTED_URL=https://pub.flutter-io.cn
export FLUTTER_STORAGE_BASE_URL=https://storage.flutter-io.cn
export PATH=$HOME/projects/dart/flutter/bin:$PATH
export PATH="$HOME/projects/sdk/android/platform-tools:$HOME/projects/sdk/android/tools:$PATH" # configure adb path

export ANDROID_HOME=$HOME/projects/sdk/android
export ANDROID_SDK_ROOT=$ANDROID_HOME

export CLASSPATH="$CLASSPATH:."
