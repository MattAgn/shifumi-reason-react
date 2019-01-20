open ReasonReact;

type sign =
  | NO_SIGN
  | ROCK
  | PAPER
  | SCISSORS;

let signs = [|ROCK, PAPER, SCISSORS|];

/* State declaration */
type state = {
  wins: int,
  losses: int,
  signPlayed: sign,
  signOpponent: sign,
  result: string,
};

/* Action declaration */
type action =
  | GetSignPlayed(sign)
  | GetSignOpponent(sign)
  | Play(sign)
  | Win
  | Lose
  | Equality;

let stringOfSign = sign =>
  switch (sign) {
  | ROCK => "ROCK"
  | PAPER => "PAPER"
  | SCISSORS => "SCISSORS"
  | NO_SIGN => "no sign"
  };

let showImg = signToDisplay =>
  switch (signToDisplay) {
  | NO_SIGN => <div />
  | _ as sign => <img src={"../src/img/" ++ stringOfSign(sign) ++ ".png"} />
  };

let getResult = (winAgainst, loseAgainst, signOpponent, self) =>
  if (winAgainst == signOpponent) {
    self.send(Win);
  } else if (loseAgainst == signOpponent) {
    self.send(Lose);
  } else {
    self.send(Equality);
  };

let play = (sign, self) => {
  self.send(GetSignPlayed(sign));
  let signOpponent = signs[Random.int(3)];
  self.send(GetSignOpponent(signOpponent));
  switch (sign) {
  | ROCK => getResult(SCISSORS, PAPER, signOpponent, self)
  | PAPER => getResult(ROCK, SCISSORS, signOpponent, self)
  | SCISSORS => getResult(PAPER, ROCK, signOpponent, self)
  | _ => ()
  };
};

/* Component template declaration.
   Needs to be **after** state and action declarations! */
let component = ReasonReact.reducerComponent("App");

/* greeting and children are props. `children` isn't used, therefore ignored.
   We ignore it by prepending it with an underscore */
let make = _children => {
  /* spread the other default fields of component here and override a few */
  ...component,
  initialState: () => {
    wins: 0,
    losses: 0,
    result: "",
    signPlayed: NO_SIGN,
    signOpponent: NO_SIGN,
  },
  /* State transitions */
  reducer: (action, state) =>
    switch (action) {
    | Win =>
      Update({...state, wins: state.wins + 1, result: "Congrats you win !"})
    | Lose => Update({...state, losses: state.losses + 1, result: "You lose"})
    | Equality => Update({...state, result: "Equality"})
    | GetSignPlayed(sign) => Update({...state, signPlayed: sign})
    | GetSignOpponent(sign) => Update({...state, signOpponent: sign})
    | Play(signPlayed) => SideEffects(play(signPlayed))
    },
  render: self =>
    <div>
      <h1> {string("Shifumi with ReasonReact")} </h1>
      <div className="row">
        <div>
          <button onClick={_event => self.send(Play(ROCK))}>
            {string("ROCK")}
          </button>
          <button onClick={_event => self.send(Play(PAPER))}>
            {string("PAPER")}
          </button>
          <button onClick={_event => self.send(Play(SCISSORS))}>
            {string("SCISSORS")}
          </button>
        </div>
        <p> {string(self.state.result)} </p>
      </div>
      <div className="container">
        <div className="col">
          {showImg(self.state.signPlayed)}
          <h3> {string("Player: " ++ string_of_int(self.state.wins))} </h3>
        </div>
        <div className="col">
          {showImg(self.state.signOpponent)}
          <h3> {string("IA: " ++ string_of_int(self.state.losses))} </h3>
        </div>
      </div>
    </div>,
};