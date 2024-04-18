using UnityEngine;
using UnityEngine.SceneManagement;

namespace LevelArtInitialLHQ
{
    public class SceneLoader : MonoBehaviour
    {
        // 通过Inspector暴露LoadTime参数，单位是秒
        public float LoadTime = 5.0f;

        // 用于计时
        private float timer = 0;

        // FixedUpdate用于物理更新，但也可以用来精确地计算时间
        void FixedUpdate()
        {
            // 累加上一帧到这一帧的时间
            timer += Time.fixedDeltaTime;

            // 当累加时间达到设定的LoadTime时，加载下一个场景
            if (timer >= LoadTime)
            {
                LoadNextScene();
            }
        }

        void LoadNextScene()
        {
            // 获取当前场景的索引
            int currentSceneIndex = SceneManager.GetActiveScene().buildIndex;
            // 计算下一个场景的索引。如果当前场景是Build Settings中的最后一个场景，则加载第一个场景
            int nextSceneIndex = (currentSceneIndex + 1) % SceneManager.sceneCountInBuildSettings;
            // 加载下一个场景
            SceneManager.LoadScene(nextSceneIndex);
        }
    }
}

