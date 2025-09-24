import { defineConfig, loadEnv } from "vite";
import react from "@vitejs/plugin-react";
import path from "node:path";
import { fileURLToPath } from "node:url";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
// https://vite.dev/config/
export default defineConfig(({ mode }) => {
  const envDir = path.resolve(__dirname, ".."); // parent folder of frontend
  const env = loadEnv(mode, envDir, "");
  console.log("Vite env:", env.VITE_BASE);
  return {
    base: env.VITE_BASE || "/",
    plugins: [react()],
    server: {
      proxy: {
        "/api": {
          target: "http://localhost:8080",
          changeOrigin: true,
        },
      },
    },
  };
});
