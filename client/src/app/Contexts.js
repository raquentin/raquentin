import { createContext } from 'react';

export const ThemeContext = createContext({
    isLightMode: false,
    setIsLightMode: () => {}
});