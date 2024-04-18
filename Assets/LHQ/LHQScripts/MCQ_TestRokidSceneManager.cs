using System;
using System.Collections;
using System.Xml;
using Unity.VisualScripting;
using UnityEngine;
using UnityEngine.SceneManagement;

public class MCQ_TestRokidSceneManager : MonoBehaviour
{
    public static MCQ_TestRokidSceneManager Instance { get; private set; }

    private void Awake()
    {
        if (MCQ_TestRokidSceneManager.Instance != null)
        {
            GameObject.Destroy(this.gameObject);
            return;
        }
        Instance = this;
        Init();
    }


    int MaxSceneCount;
    int CurSceneCount = 0;

    private void Init()
    {
        MaxSceneCount = SceneManager.sceneCountInBuildSettings;
        CurSceneCount = 0;
        SceneManager.activeSceneChanged += ActiveSceneChanged;
        LoadState = 0;
        Debug.Log("获取总场景数量为 : " + MaxSceneCount);
        DontDestroyOnLoad(this.gameObject);
    }
    // 0 默认状态. 1 加载中, 2 加载完成
    int LoadState;

    private void Update()
    {
        if(Input.GetKeyDown(KeyCode.UpArrow) &&LoadState == 0)
        {
            CurSceneCount++;
            if (CurSceneCount >= MaxSceneCount)
                CurSceneCount = 0;
            LoadAssginScene(CurSceneCount, null);
        }
        else if (Input.GetKeyDown(KeyCode.DownArrow) && LoadState == 0)
        {
            CurSceneCount--;
            if (CurSceneCount < 0)
                CurSceneCount = MaxSceneCount - 1;
            LoadAssginScene(CurSceneCount, null);
        }
        if (LoadState == 2)
        {
            LoadState = 0;
            NowSkipLoadedScene();
        }
    }
    /// <summary>
    /// 加载指定场景
    /// </summary>
    /// <param name="SceneIndex">场景下表</param>
    /// <param name="loaded">完成回调</param>
    public void LoadAssginScene(int SceneIndex, Action loaded)
    {
        if (LoadState != 0)
            return;
        LoadState = 1;
        //加载一个新场景
        StartCoroutine(StartLoadScene(SceneIndex, loaded));
    }
    AsyncOperation LoadedSceneAsync;
    Scene OldScene;

    IEnumerator StartLoadScene(int sceneIndex, Action loaded)
    {
        //获取当前的场景
        OldScene = SceneManager.GetActiveScene();
        LoadedSceneAsync = SceneManager.LoadSceneAsync(sceneIndex);
        LoadedSceneAsync.allowSceneActivation = false;
        while (LoadedSceneAsync.progress < .8f)
        {
            yield return null;
        }
        Debug.Log("场景加载完成了");
        LoadState = 2;
    }
   
    public void NowSkipLoadedScene()
    {
        LoadedSceneAsync.allowSceneActivation = true;
    }

    /// <summary>
    /// 活动的场景已经变更了
    /// </summary>
    /// <param name="arg0"></param>
    /// <param name="arg1"></param>
    void ActiveSceneChanged(Scene arg0, Scene arg1)
    {
        //重置掉函数
        LoadedSceneAsync = null;
    }
}
