const {app, BrowserWindow, globalShortcut} = require('electron');
const freeport = require('freeport');
const spawn    = require('child_process').spawn;
const path     = require('path');
const waitOn   = require('wait-on');
const { initialize, enable } = require("@electron/remote/main");

initialize();

 // Time to wait for Threepenny server, milliseconds
const timeout = 10000;
// Relative path to the Threepenny binary.
const relBin = './dist-newstyle/build/x86_64-linux/ghc-9.4.8/tidal-gui-0.1.0.0/x/tidal-gui/build/tidal-gui/tidal-gui';

// Assign a random port to run on.
freeport((err, port) => {
  if (err) throw err;

  const url = `http://localhost:8023`;
  let child = null; // the Threepenny Server process we will spawn

  // Keep a global reference of the window object, if we don't, the window will
  // be closed automatically when the JavaScript object is garbage collected.
  let win;

  // Called when Electron has finished initialization and is ready to create
  // browser windows. Some APIs can only be used after this event occurs. We
  // start the child process and wait before loading the web page.
  app.on('ready', () => {
    child = spawn(path.join(__dirname, relBin), [8023]);
    child.stdout.setEncoding('utf8');
    child.stderr.setEncoding('utf8');
    child.stdout.on('data', console.log);
    child.stderr.on('data', console.log);
    child.on('close', code =>
      console.log(`Threepenny app exited with code ${code}`));

    // Wait until the Threepenny server is ready for connections.
    waitOn({ resources: [url], timeout }, (err_) => {
      if (err_) throw err_;
      createWindow();
    });

      //unregister all shortcuts coming from electron for the defined shortcuts

    globalShortcut.unregisterAll();
  });

  function createWindow() {
      // Create the browser window.
      win = new BrowserWindow({
          width: 470,
          height: 370,
          maximizable: true,
          resizable: true,
          title: 'TidalCycles',
          webPreferences: {nodeIntegration: true, contextIsolation: false,
  nativeWindowOpen: true, enableRemoteModule: true}
      });

      enable(win.webContents);

      //win.removeMenu();
      console.log(`Loading URL: ${url}`);
      win.loadURL(url);

      //Emitted when the window is closed.
      win.on('closed', () => {
          // Dereference the window object for garbage collection.
          win = null;
      });
  }

  // Quit when all windows are closed, unless on macOS. On macOS it is common
  // for applications and their menu bar to stay active until the user quits
  // explicitly with Cmd + Q
  app.on('window-all-closed', () => {
    if (process.platform !== 'darwin') {
      app.quit();
    }
  });



  // Kill the child process when quitting Electron.
  app.on('will-quit', () => child.kill());

  app.on('activate', () => {
    // On macOS it's common to re-create a window in the app when the dock icon
    // is clicked and there are no other windows open.
    if (win === null) {
      createWindow();
    }
  });
});
