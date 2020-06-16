const ERRORS = {
    out_of_range: 1,
    impossible_move: 2
};


class MathUtils {
    static getRndInteger(min, max) {
        return Math.floor(Math.random() * (max - min + 1)) + min;
    }
}

class HumanPlayer {
    makeMove(board) {

        while (true) {
            let x = parseInt(prompt(`${board.player === PLAYERS.FIRST ? "First" : "Second"} player turn. Enter your x-axis move`));

            if (isNaN(x)) {
                alert('Incorrect input. Try again')
            } else {
                let res = board.makeMove(x - 1);
                switch (res) {
                    case ERRORS.impossible_move:
                    case ERRORS.out_of_range:
                        alert('Incorrect input. Try again');
                        break;
                    case 0:
                        return
                }

            }
        }
    }
}

class ComputerPlayer {
    makeMove(board) {
        console.log("Computer turn");
        let a = MathUtils.getRndInteger(0, board.xSizeBoard);
        while (board.makeMove(a) !== 0) {
            a = MathUtils.getRndInteger(0, board.xSizeBoard);
        }
        console.log(`Computer move is ${a}`);
    }
}

const PLAYERS = {
    NONE: 0,
    FIRST: 1,
    SECOND: 2
};

class BoardConfiguration {
    xSizeBoard = 7;
    ySizeBoard = 6;
    winSeries = 4;
    board;
    player = PLAYERS.FIRST;
    winner = PLAYERS.NONE;

    constructor() {
        this.board = [];
        for (let i = 0; i < this.ySizeBoard; i++) {
            this.board[i] = Array(this.xSizeBoard).fill(0)
        }
    }

    makeMove(x) {
        const errorCode = this.validateMove(x);

        if (!errorCode) {
            for (let i = 0; i < this.ySizeBoard; i++) {
                if (!this.board[i][x]) {
                    this.board[i][x] = this.player;
                    break;
                }
            }

            this.checkWinCondition();

            this.changePlayerTurn();

            return 0;
        } else {
            return errorCode;
        }
    }

    changePlayerTurn() {
        this.player = this.player === PLAYERS.FIRST ? PLAYERS.SECOND : PLAYERS.FIRST;
    }


    validateMove(x) {
        if (x < 0 || x >= this.xSizeBoard) {
            return ERRORS.out_of_range;
        }

        for (let i = 0; i < this.ySizeBoard; i++) {
            if (!this.board[i][x]) {
                break;
            }

            if (i === this.ySizeBoard - 1) {
                return ERRORS.impossible_move;
            }
        }

        return 0;
    }

    checkWinCondition() {
        for (let i = 0; i < this.ySizeBoard; i++) {
            for (let j = 0; j < this.xSizeBoard; j++) {
                if (this.board[i][j] !== 0 && i + this.winSeries < this.ySizeBoard) {
                    let flag = true;
                    for (let k = 1; k <= this.winSeries - 1; k++) {
                        if (this.board[i][j] !== this.board[i + k][j]) flag = false;
                    }
                    if (flag) {
                        this.winner = this.player;
                    }
                }

                if (this.board[i][j] !== 0 && j + this.winSeries < this.xSizeBoard) {
                    let flag = true;
                    for (let k = 1; k <= this.winSeries - 1; k++) {
                        if (this.board[i][j] !== this.board[i][j + k]) flag = false;
                    }
                    if (flag) {
                        this.winner = this.player;
                    }

                }

                if (this.board[i][j] !== 0 && i + this.winSeries <= this.ySizeBoard && j + this.winSeries < this.xSizeBoard) {
                    let flag = true;
                    for (let k = 1; k <= this.winSeries - 1; k++) {
                        if (this.board[i][j] !== this.board[i + k][j + k]) flag = false;
                    }
                    if (flag) {
                        this.winner = this.player;
                    }
                }

                if (this.board[i][j] !== 0 && i + this.winSeries <= this.ySizeBoard && j - this.winSeries >= -1) {
                    let flag = true;
                    for (let k = 1; k <= this.winSeries - 1; k++) {
                        if (this.board[i][j] !== this.board[i + k][j - k]) flag = false;
                    }
                    if (flag) {
                        this.winner = this.player;
                    }
                }
            }
        }
    }

}

class GameServer {
    game = new BoardConfiguration();

    startGame(player1, player2) {
        while (true) {
            this.printBoard();

            if (this.game.player === PLAYERS.FIRST) {
                player1.makeMove(this.game);
            } else if (this.game.player === PLAYERS.SECOND) {
                player2.makeMove(this.game);
            }

            if (this.game.winner !== PLAYERS.NONE) {
                this.printBoard();
                console.log(`${this.game.winner === PLAYERS.FIRST ? "First" : "Second"} player won`);
                break;
            }
        }
    }

    printBoard() {
        let a = " ";
        for (let i = this.game.ySizeBoard - 1; i >= 0; i--) {
            let line = "";
            for (let j = 0; j < this.game.xSizeBoard; j++) {
                switch (this.game.board[i][j]) {
                    case PLAYERS.NONE:
                        a = '.';
                        break;
                    case PLAYERS.FIRST:
                        a = 'X';
                        break;
                    case PLAYERS.SECOND:
                        a = 'O';
                        break;
                    default:
                }
                line += a + " ";
            }
            console.log(line);
        }
    }

}


server = new GameServer();
server.startGame(new HumanPlayer(), new ComputerPlayer());