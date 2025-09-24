import { Suspense, lazy } from "react";
import {
  ThemeProvider,
  createTheme,
  useColorScheme,
} from "@mui/material/styles";
import CssBaseline from "@mui/material/CssBaseline";
import {
  Box,
  CircularProgress,
  AppBar,
  IconButton,
  Toolbar,
  Typography,
} from "@mui/material";
import { BrowserRouter as Router, Routes, Route } from "react-router-dom";
//lazy import of home
//import Home from "./components/Home";
//import Foo from "./components/Foo";
import { MathJaxContext } from "better-react-mathjax";
import { DarkMode, LightMode } from "@mui/icons-material";

const LoadingFallback = () => (
  <Box
    sx={{
      height: "100vh",
      display: "flex",
      alignItems: "center",
      justifyContent: "center",
      flexDirection: "column",
    }}
  >
    <CircularProgress />
    <Typography variant="subtitle1" sx={{ mt: 2 }}>
      Loading content...
    </Typography>
  </Box>
);

const Home = lazy(() => import("./components/Home"));
const Foo = lazy(() => import("./components/Foo"));

function App() {
  const { mode, setMode } = useColorScheme();
  if (!mode) {
    return null;
  }
  return (
    <MathJaxContext
      version={3}
      config={{
        loader: { load: ['[tex]/color', "[tex]/bbox"] },
        tex: {
          packages: {'[+]': ['color']},
          macros: {
            /* Define your global macros here */
          },
        },
      }}
    >
      <Router>
        <CssBaseline />
        <div className="main">
          <AppBar position="static">
            <Toolbar>
              <Typography variant="h6" sx={{ flexGrow: 1 }}>
                Gredex
              </Typography>

              <IconButton
                aria-label="theme"
                aria-controls="menu-appbar"
                aria-haspopup="true"
                color="inherit"
                onClick={(_) => setMode(mode === "dark" ? "light" : "dark")}
              >
                {mode === "light" ? <LightMode /> : <DarkMode />}
              </IconButton>
            </Toolbar>
          </AppBar>

          <div className="content">
            <Suspense fallback={<LoadingFallback />}>
              <Routes>
                <Route path="/" element={<Home />} />
                <Route path="/:example" element={<Home />} />
                <Route path="/foo" element={<Foo />} />
              </Routes>
            </Suspense>
          </div>
        </div>
      </Router>
    </MathJaxContext>
  );
}

const theme = createTheme({
  colorSchemes: {
    dark: true,
  },
});

export default function ToggleColorMode() {
  return (
    <ThemeProvider theme={theme}>
      <App />
    </ThemeProvider>
  );
}
