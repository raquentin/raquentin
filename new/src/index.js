import React from 'react';
import ReactDOM from 'react-dom/client';
import Root from './App';

import './common/global.css'

const root = ReactDOM.createRoot(document.getElementById('root'));
root.render(
  <React.StrictMode>
    <Root />
  </React.StrictMode>
);