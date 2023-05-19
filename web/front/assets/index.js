(async () => {
  const INIT_MESSAGE = 'init';
  const STATE_MESSAGE = 'state';
  const MOVE_MESSAGE = 'move';
  const INVALID_MOVE_MESSAGE = 'invalid_move';
  const WINNER_MESSAGE = 'winner';
  const TURN_MESSAGE = 'turn';

  const create_json_message = (type, data) => ({ type, data });

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
    socket.addEventListener('open', () => {
      console.log('Connected to websocket');
      socket.send(JSON.stringify(create_json_message(INIT_MESSAGE, messageData)));
    });

    let table_locked = true;
    socket.addEventListener('message', (event) => {
      const message = JSON.parse(event.data).data;
      console.log('Message from server ', message);

      if (message.type === STATE_MESSAGE) {
        gameContainer.innerHTML = '';
        const table = document.createElement('table');
        gameContainer.appendChild(table);

        message.data.forEach((row) => {
          const tableRow = document.createElement('tr');
          table.appendChild(tableRow);
          row.forEach((cell, index) => {
            const tableCell = document.createElement('td');
            if (cell === 'r') {
              tableCell.className = 'red';
            } else if (cell === 'b') {
              tableCell.className = 'blue';
            }
            tableCell.addEventListener('click', () => {
              if (!table_locked) {
                console.log('Sending message');
                socket.send(JSON.stringify(create_json_message(MOVE_MESSAGE, index)));
                table_locked = true;
              }
            });
            tableRow.appendChild(tableCell);
          });
        });
      } else if (message.type === INVALID_MOVE_MESSAGE) {
        alert('Invalid move');
        table_locked = false;
      } else if (message.type === TURN_MESSAGE) {
        table_locked = false;
      } else if (message.type === WINNER_MESSAGE) {
        alert({ r: 'Red wins', b: 'Blue wins', tie: "It's a tie" }[message.data]);
        table_locked = true;
      }
    });
  });
})();
