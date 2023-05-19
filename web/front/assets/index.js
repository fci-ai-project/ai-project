(async () => {
  INIT_MESSAGE = 'init';
  STATE_MESSAGE = 'state';

  const gameContainer = document.getElementById('gameContainer');
  const form = document.getElementsByTagName('form')[0];

  form.addEventListener('submit', async (e) => {
    e.preventDefault();
    const messageData = Object.fromEntries(new FormData(form));
    ['columns', 'max_plies', 'rows', 'winning_factor'].forEach(
      (name) => (messageData[name] = parseInt(messageData[name]))
    );
    const socket = new WebSocket('ws://localhost:2002/play');
    socket.onerror = (e) => alert(e);
    socket.addEventListener("open", () => {
      console.log('Connected to websocket');
      socket.send(JSON.stringify({ type: INIT_MESSAGE, data: messageData }));
    });

    socket.addEventListener('message', (event) => {
      const message = JSON.parse(event.data).data;
      console.log('Message from server ', message);

      if (message.type === 'state') {
        gameContainer.innerHTML = '';
        const table = document.createElement('table');
        gameContainer.appendChild(table);

        message.data.forEach((row) => {
          const tableRow = document.createElement('tr');
          table.appendChild(tableRow);
          row.forEach((cell) => {
            const tableCell = document.createElement('td');
            if (cell === 'r') {
              tableCell.className = 'red';
            } else if (cell === 'b') {
              tableCell.className = 'blue';
            }
            tableRow.appendChild(tableCell);
          });
        });
      }
    });
  });
})();
