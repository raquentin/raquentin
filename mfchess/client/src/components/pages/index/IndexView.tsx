//* import third-party deps
import { Link } from "react-router-dom";
import styled from "styled-components";

//* import local
import ViewWrapper from "components/common/ViewWrapper";
import LogoPNG from "assets/logo.png";
import { useUser } from "context/UserContext";
import LogInOutButton from "./LogInOutButton";
import { statusEnum, useGame } from "context/WebSocket";
import {useNavigate} from 'react-router-dom';

/*
 * IndexView is the head component for the index page (mfchess.com/)
 @returns JSX.element jsx structure for the index page
*/
const IndexView = (): JSX.Element => {

  const [user, ,] = useUser()
  const [isConnected, status, , color, chess, opponent, sendMessage, makeMove, clearLocalStorage] = useGame()
  
  const navigate = useNavigate();

  const playOnClick = () => {
    console.log("SSSTATUS:", status)
    if (status >= statusEnum.Authenticated) {

      navigate('/game', {replace: true});
      
    } else {
      alert("Not connected or not authenticated")
      if (user.loggedIn) {
        sendMessage({
          type: "upgrade status",
          payload: {
            name: "authentication",
            userID: "",
            data: user.jwtCredential,
          }
        })
      }
    }
  }
  //* render
  return (
    <ViewWrapper> {/** holds animation and container logic*/}
      <PageContainer>
        <LogoImage src={LogoPNG} />
        <RightSideContainer>
          <TitleText>mfChess</TitleText>
          <ButtonContainer>
            <PlayButton onClick={playOnClick}>play</PlayButton>
            <LogInOutButton />
            {/* <PlayButton to="/chatroom">chatroom</PlayButton> */}
          </ButtonContainer>
        </RightSideContainer>
      </PageContainer>
    </ViewWrapper>
  );
}

export default IndexView;

//lines below this point are styled-components logic

/*
 * PageContainer is the flexbox that wraps the two sides of the screen
*/
export const PageContainer = styled.div`
  width: 100%;
  height: 97vh;
  gap: 5em;
  display: flex;
  justify-content: center;
  align-items: center;
`;

/*
 * ContentRow is the flexbox that wraps the two sides of the screen
*/
export const ContentRow = styled.div`
  width: 100%;
  display: flex;
  flex-direction: column;
  justify-content: center;
  gap: 5em;
  align-items: center;
`;

/*
 * Logo is the mfChess logo on the left
*/
export const LogoImage = styled.img`
  height: 40vh;
`;

/*
 * RightSideContainer is the flexbox on the right that aligns the mfChess title and the play button
*/
export const RightSideContainer = styled.div`
  display: flex;
  gap: 2em;
  flex-direction: column;
  align-items: center;
  justify-content: center;
`;

/*
 * TitleText is the mfChess title
*/
export const TitleText = styled.h1`
  color: white;
  font-size: 10em;
  font-weight: 800;
  margin: 0;
`;

/*
 * ButtonContainer is the flexbox on the right that aligns the mfChess title and the play button
*/
export const ButtonContainer = styled.div`
  display: flex;
  gap: 2em;
  align-items: center;
  justify-content: center;
`;

/*
 * PlayButton is the play button that extends the react-router-dom Link component
 TODO: make the hover effect better
 ! Update LogInOutButton if you change this.
 TODO: Modularize this so you dont have to manually update
*/
export const PlayButton = styled.div`
  background-color: #333333;
  border: 0.22em solid black;
  border-radius: 14px;
  color: white;
  font-size: 3em;
  font-weight: 800;
  text-decoration: none;
  padding: 0.25em 0.5em;

  &:hover {
    background-color: #287485;
  }
`;

// transition: all 0.3s ease;
