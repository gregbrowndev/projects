import {Component} from "@angular/core";

@Component({
  selector: 'app-server',
  templateUrl: './server.component.html'
})
export class ServerComponent {
  serverName: any = '';
  allowNewServer = false;
  serverCreationStatus = 'No server was created!';
  servers = ['testserver', 'testserver 2'];

  constructor() {
    setTimeout(() => {
      this.allowNewServer = true;
    }, 2000);
  }

  onCreateServer() {
    // this.servers.push(this.serverName);
    this.serverCreationStatus = 'Server was created! Name is ' + this.serverName;
  }

  onUpdateServerName($event: Event) {
    this.serverName = (<HTMLInputElement>event.target).value;
  }
}

